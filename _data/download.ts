/* This script downloads all erlang/otp releases and then
 * groups them by major release and outputs them to the file
 * given as the argument.
 */

const fs = require('fs');
import { Octokit } from "@octokit/rest";
import {
    GetResponseTypeFromEndpointMethod,
    GetResponseDataTypeFromEndpointMethod,
} from "@octokit/types";
import { release } from "os";

const octokit = new Octokit({ auth: process.env.GITHUB_TOKEN });

type ListReleases = GetResponseDataTypeFromEndpointMethod<
    typeof octokit.rest.repos.listReleases
>;

type Release = GetResponseDataTypeFromEndpointMethod<
    typeof octokit.rest.repos.getRelease
>;

export async function getAllReleases(): Promise<ListReleases> {

    if (!process.env.GITHUB_TOKEN) {
        console.warn(`Running without github authentication, 
consider setting GITHUB_TOKEN in order for the API to not throttle you.`)
    }

    let releases: ListReleases = await octokit.paginate(
        octokit.rest.repos.listReleases,
        { owner: "erlang", repo: "otp" });
    releases.sort((a, b) => {
        const vsn_match = /^OTP-([0-9]+)\.([0-9]+)(\.[0-9]+)?(\.[0-9]+)?(\.[0-9]+)?(\.[0-9]+)?/;
        const a_match = a.tag_name.match(vsn_match);
        const b_match = b.tag_name.match(vsn_match);
        for (var i = 1; i < 7; i++) {
            if (a_match?.[i] == b_match?.[i])
                continue;
            if (a_match?.[i] == undefined)
                return 1;
            if (b_match?.[i] == undefined)
                return -1;
            if (b_match?.[i] < a_match[i])
                return -1;
            return 1;
        }
        return 0;
    });
    return releases;
}

getAllReleases().then(releases => {
    let id = 0;
    type Patch = {
        name: string,
        tag_name: string,
        html_url: string,
        readme?: string,
        html?: string,
        man?: string,
        win32?: string,
        win64?: string,
        src?: string
    };
    type Release = {
        latest: Patch,
        release: string,
        patches: Patch[]
    };
    const majorReleases: Release[] = [];
    releases.forEach(release => {

        const majorReleaseVsn = release.tag_name.match(/^OTP-([0-9]+)/)?.[1]!;
        let patch: Patch = {
            name: release.tag_name.match(/^OTP-(.*)/)?.[1]!,
            tag_name: release.tag_name,
            html_url: release.html_url
        };
        let readmeFileId: number | undefined = undefined;
        let majorRelease = majorReleases.find((v) => {
            return v.release == majorReleaseVsn;
        });
        if (!majorRelease) {
            majorRelease = {
                release: majorReleaseVsn,
                latest: patch,
                patches: []
            };
            majorReleases.push(majorRelease as Release);
        }

        majorRelease.patches.push(patch);

        const keys = ["html_url", "tag_name", "name", "published_at"];

        let frontMatter = "layout: release\n";
        frontMatter += "release: " + majorReleaseVsn + "\n";
        release.assets.forEach(asset => {
            if (asset.name.match(/^OTP-.*\.README$/)) {
                readmeFileId = asset.id;
                frontMatter += "readme: ";
                patch.readme = asset.browser_download_url;
            } else if (asset.name.match(/^otp_doc_html.*/)) {
                frontMatter += "html: ";
                patch.html = asset.browser_download_url;
            } else if (asset.name.match(/^otp_doc_man.*/)) {
                frontMatter += "man: ";
                patch.man = asset.browser_download_url;
            } else if (asset.name.match(/^otp_win32.*/)) {
                frontMatter += "win32: ";
                patch.win32 = asset.browser_download_url;
            } else if (asset.name.match(/^otp_win64.*/)) {
                frontMatter += "win64: ";
                patch.win64 = asset.browser_download_url;
            } else if (asset.name.match(/^otp_src.*/)) {
                frontMatter += "src: ";
                patch.src = asset.browser_download_url;
            } else {
                return;
            }

            frontMatter += asset.browser_download_url + "\n";

        });

        keys.forEach(key => {
            frontMatter += key + ": " + release[key] + "\n";
        });

        octokit.rest.repos.getReleaseAsset({
            headers: {
                accept: "application/octet-stream"
            },
            owner: "erlang",
            repo: "otp",
            asset_id: readmeFileId!
        }).then(readmeFile => {
            let buffer = readmeFile.data as unknown as ArrayBuffer;
            const releaseFile = "---\n" + frontMatter + "---\n" +
                "```\n" + new TextDecoder().decode(buffer) + "```";
            fs.writeFileSync(process.argv[3] + "/" + release.tag_name + ".md",
                releaseFile);
        }).catch(err => {
            console.log("Failed to get " + release.tag_name);
        });
    });

    // majorReleases.forEach(release => {

    //     let frontMatter = "layout: release\n";
    //     frontMatter += "release: " + release.release + "\n";

    //     fs.writeFileSync(process.argv[3] + "/OTP-" + release.release + ".md",
    //         "---\n" + frontMatter + "---\n");
    // });

    fs.writeFileSync(process.argv[2],
        JSON.stringify(majorReleases, null, 2));

}).catch(err => {
    console.log(err);
});

