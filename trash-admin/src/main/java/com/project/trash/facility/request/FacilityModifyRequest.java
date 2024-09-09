package com.project.trash.facility.request;

import java.math.BigDecimal;
import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 시설물 수정 요청
 */
@Getter
public class FacilityModifyRequest {

  /**
   * 시설물 일련번호
   */
  @Schema(description = "시설물 일련번호", example = "1")
  private Long facilitySeq;
  /**
   * 시설물 종류
   */
  @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;
  /**
   * 시설물명
   */
  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;
  /**
   * 위치
   */
  @Schema(description = "위치", example = "쌍문역")
  private String location;
  /**
   * 상세 위치
   */
  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;
  /**
   * 위도
   */
  @Schema(description = "위도", example = "2.6112336")
  private BigDecimal latitude;
  /**
   * 경도
   */
  @Schema(description = "경도", example = "33.126115")
  private BigDecimal longitude;
  /**
   * 정보
   */
  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;
  /**
   * 관리 부서
   */
  @Schema(description = "관리 부서", example = "서울시설공단")
  private String department;
  /**
   * 관리 부서 전화번호
   */
  @Schema(description = "관리 부서 전화번호", example = "02-2290-7111")
  private String departmentPhoneNumber;
  /**
   * 승인 상태
   */
  @Schema(description = "승인 상태 (P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;
}
