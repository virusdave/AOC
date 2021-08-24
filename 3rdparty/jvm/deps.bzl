
load("@rules_jvm_external//:defs.bzl", "maven_install")

def _deps():
    return [
        "junit:junit:4.13.2",
        #"androidx.test.espresso:espresso-core:3.1.1",
        "org.hamcrest:hamcrest-library:2.2",
    ]

def jvm_dependencies():
    maven_install(
        name = "maven",
        artifacts = _deps(),
        repositories = [
            "https://maven.google.com",
            "https://repo1.maven.org/maven2",
        ],
        fetch_sources = True,
        maven_install_json = "@//3rdparty/jvm:pinned_maven_install.json",
    )