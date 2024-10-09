package com.project.trash.member.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class MemberDeleteRequest {

  @Schema(description = "엑세스 토큰", example = "AAAAOQyhkOCIHqxBRocc56YEJ5txff0nDZxWURIixTjytcKFsQcQgCv-y6MiC8ObOrmiJfWHMwFn-Y6JsXYm-oiMXrE")
  private String accessToken;
}
