package com.project.trash.admin.request;

import lombok.Getter;

/**
 * 관리자 수정 요청
 */
@Getter
public class AdminModifyRequest {

  /**
   * 변경할 비밀번호
   */
  private String password;
}
