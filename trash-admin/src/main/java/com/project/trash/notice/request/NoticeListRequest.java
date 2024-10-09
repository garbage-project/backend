package com.project.trash.notice.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class NoticeListRequest extends PageRequest {

  @Parameter(description = "공지 제목", example = "공지사항")
  private String title;

  @Parameter(description = "공지 노출여부(Y - 노출, N - 미노출)", example = "Y")
  private String valid;

  @Parameter(description = "공지 생성일 검색 시작일 (yyyy-MM-dd)", example = "2024-06-01")
  private String startDate;

  @Parameter(description = "공지 생성일 검색 종료일 (yyyy-MM-dd)", example = "2024-09-08")
  private String endDate;
}
