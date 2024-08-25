package com.project.trash.notice.request;

import com.project.trash.common.request.PageRequest;

import lombok.Getter;
import lombok.Setter;

/**
 * 공지 목록 조회 요청
 */
@Getter
@Setter
public class NoticeListRequest extends PageRequest {

  /**
   * 제목
   */
  private String title;
  /**
   * 유효여부
   */
  private String valid;
  /**
   * 생성일 검색 시작일
   */
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  private String endDate;
}
