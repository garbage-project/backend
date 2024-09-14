package com.project.trash.facility.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;
import com.project.trash.member.domain.Member;

import org.jooq.types.ULong;

import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 상세 조회 응답
 */
@Getter
@Setter
@Schema(title = "시설물 상세 조회 응답")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FacilityDetailResponse {

  /**
   * 시설물 ID
   */
  @Schema(description = "시설물 ID", example = "1")
  private Long facilityId;
  /**
   * 시설물 종류
   */
  @Schema(description = "시설물 종류 (R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
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
   * 정보
   */
  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;
  /**
   * 관리 부서
   */
  @Schema(description = "관리 부서", example = "서울시설공단", nullable = true)
  private String department;
  /**
   * 관리 부서 전화번호
   */
  @Schema(description = "관리 부서 전화번호", example = "02-2290-7111", nullable = true)
  private String departmentPhoneNumber;
  /**
   * 승인 상태
   */
  @Schema(description = "승인 상태 (P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;
  /**
   * 등록자 ID
   */
  @Schema(description = "등록자 ID", example = "test1234")
  private String memberId;
  /**
   * 등록일자
   */
  @Schema(description = "시설물 등록일자", example = "2024-09-01")
  private String createdDate;
  /**
   * 이미지 목록
   */
  @Schema(description = "이미지 목록", example = "[\"https://spotfinder-image.s3.ap-northeast-2.amazonaws.com/facility/2024/09/14/4-222150239.png\"]")
  private List<String> images;

  public FacilityDetailResponse(Facility facility, String s3ImageUrl) {
    this.facilityId = facility.getFacilityId();
    this.type = facility.getType().getCode();
    this.name = facility.getName();
    this.location = facility.getLocation();
    this.detailLocation = facility.getDetailLocation();
    this.information = facility.getInformation();
    this.department = facility.getDepartment();
    this.departmentPhoneNumber = facility.getDepartmentPhoneNumber();
    this.approvalStatus = facility.getApprovalStatus().getCode();
    this.memberId = facility.getMemberId();
    this.createdDate = DateTimeUtils.convertToString(facility.getCreatedAt(), DateTimeUtils.DEFAULT_DATE);
    this.images = facility.getImages()
        .stream()
        .map(image -> s3ImageUrl + image.getPath())
        .toList();
  }
}
