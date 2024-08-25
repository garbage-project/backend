package com.project.trash.report.request;

import com.project.trash.common.request.PageRequest;

import lombok.Getter;
import lombok.Setter;

/**
 * 신고 목록 조회 요청
 */
@Getter
@Setter
public class ReportListRequest extends PageRequest {

  /**
   * 신고 내용
   */
  private String content;
  /**
   * 상태
   */
  private String status;
  /**
   * 회원 일련번호(신고자 ID)
   */
  private Long memberSeq;
  /**
   * 시설물 ID
   */
  private String facilityId;
  /**
   * 생성일 검색 시작일
   */
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  private String endDate;
}
