package com.project.trash.admin.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class ReissueRequest {

  @Schema(description = "관리자 ID", example = "testid1234")
  private String id;
}
