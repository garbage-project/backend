package com.project.trash.notice.request;

import lombok.Getter;

/**
 * 공지 등록 요청
 */
@Getter
public class NoticeEntryRequest {

  /**
   * 제목
   */
  private String title;
  /**
   * 내용
   */
  private String content;
  /**
   * 유효여부
   */
  private String valid;
}
