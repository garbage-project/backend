package com.project.trash.admin.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class AdminModifyRequest {

  @Schema(description = "변경할 비밀번호", example = "update1234")
  private String password;
}
