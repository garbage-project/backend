package com.project.trash.facility.request;

import com.project.trash.common.request.PageRequest;

import java.util.Set;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 목록 요청
 */
@Getter
@Setter
public class FacilityListRequest extends PageRequest {

  /**
   * 시설물 ID
   */
  @Parameter(description = "시설물 ID", example = "1")
  private Long facilityId;
  /**
   * 시설물 종류
   */
  @Parameter(description = "시설물 종류 (R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;
  /**
   * 위치O
   */
  @Parameter(description = "위치", example = "쌍문역")
  private String location;
  /**
   * 승인 상태
   */
  @Parameter(description = "승인 상태(P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;
  /**
   * 생성일 검색 시작일
   */
  @Parameter(description = "시설물 생성일 검색 시작일 (yyyy-MM-dd)", example = "2024-06-01")
  private String startDate;
  /**
   * 생성일 검색 종료일
   */
  @Parameter(description = "시설물 생성일 검색 종료일 (yyyy-MM-dd)", example = "2024-09-08")
  private String endDate;
}
