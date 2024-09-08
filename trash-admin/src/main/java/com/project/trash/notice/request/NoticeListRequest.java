package com.project.trash.notice.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
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
  @Parameter(description = "공지 제목", example = "공지사항")
  private String title;
  /**
   * 노출여부
   */
  @Parameter(description = "공지 노출여부(Y - 노출, N - 미노출)", example = "Y")
  private String valid;
  /**
   * 생성일 검색 시작일
   */
  @Parameter(description = "공지 생성일 검색 시작일 (yyyy-MM-dd)", example = "2024-06-01")
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  @Parameter(description = "공지 생성일 검색 종료일 (yyyy-MM-dd)", example = "2024-09-08")
  private String endDate;
}
