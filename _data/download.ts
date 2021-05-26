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
    const majorReleases: number[] = [];
    releases.forEach(release => {
        const majorReleaseVsn = release.tag_name.match(/^OTP-([0-9]+)/)?.[1]!;
        majorReleases.push(parseInt(majorReleaseVsn));

        const keys = ["html_url", "tag_name", "name", "published_at"];

        let frontMatter = "";
        keys.forEach(key => {
            frontMatter += key + ": " + release[key] + "\n";
        });

        const releaseFile = "---\n" + frontMatter + "---\n" + release.body;
        fs.writeFileSync(process.argv[3] + "/" + release.tag_name + ".md",
            releaseFile);
    });

    const uniqMajorReleases = majorReleases.filter(
        (n, i) => majorReleases.indexOf(n) === i);

    fs.writeFileSync(process.argv[2],
        JSON.stringify(uniqMajorReleases, null, 2));

}).catch(err => {
    console.log(err);
});

