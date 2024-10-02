package com.project.trash.facility.response;

import com.project.trash.facility.domain.Facility;

import java.math.BigDecimal;
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
public class FacilityDetailResponse {

  @Schema(description = "시설물 ID", example = "1")
  private Long facilityId;

  @Schema(description = "시설물 종류 (R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;

  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;

  @Schema(description = "위치", example = "쌍문역")
  private String location;

  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;

  @Schema(description = "위도", example = "37.413294")
  private BigDecimal latitude;

  @Schema(description = "경도", example = "126.764166")
  private BigDecimal longitude;

  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;

  @Schema(description = "관리 부서", example = "서울시설공단", nullable = true)
  private String department;

  @Schema(description = "관리 부서 전화번호", example = "02-2290-7111", nullable = true)
  private String departmentPhoneNumber;

  @Schema(description = "승인 상태 (P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;

  @Schema(description = "이미지 목록")
  private List<Image> images;

  public FacilityDetailResponse(Facility facility, String s3ImageUrl) {
    this.facilityId = facility.getFacilityId();
    this.type = facility.getType().getCode();
    this.name = facility.getName();
    this.location = facility.getLocation();
    this.detailLocation = facility.getDetailLocation();
    this.latitude = facility.getLatitude();
    this.longitude = facility.getLongitude();
    this.information = facility.getInformation();
    this.department = facility.getDepartment();
    this.departmentPhoneNumber = facility.getDepartmentPhoneNumber();
    this.approvalStatus = facility.getApprovalStatus().getCode();
    this.images = facility.getImages()
        .stream()
        .map(image -> new Image(image.getImgId(), s3ImageUrl + image.getPath()))
        .toList();
  }

  @Getter
  @Setter
  public static class Image {

    @Schema(description = "이미지 ID", example = "1")
    private Long imageId;

    @Schema(description = "이미지 url", example = "https://spotfinder-image.s3.ap-northeast-2.amazonaws.com/facility/2024/09/14/4-222150239.png")
    private String url;

    public Image(Long imageId, String url) {
      this.imageId = imageId;
      this.url = url;
    }
  }
}
