package com.project.trash.facility.domain;

import com.project.trash.facility.domain.converter.FacilityApprovalStatusConverter;
import com.project.trash.facility.domain.converter.FacilityTypeConverter;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;

import org.bson.types.Decimal128;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.convert.ValueConverter;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 시설물 document
 */
@Getter
@NoArgsConstructor
@Document(collection = "facility")
public class Facility {

  /**
   * 시설물 ID
   */
  @Id
  private String facilityId;
  /**
   * 시설물명
   */
  private String name;
  /**
   * 시설물 종류
   */
  @ValueConverter(FacilityTypeConverter.class)
  private FacilityType type;
  /**
   * 위치
   */
  private String location;
  /**
   * 상세 위치
   */
  private String detailLocation;
  /**
   * 위도
   */
  private Decimal128 latitude;
  /**
   * 경도
   */
  private Decimal128 longitude;
  /**
   * 설명
   */
  private String information;
  /**
   * 관리 부서
   */
  private String department;
  /**
   * 관리 부서 전화번호
   */
  private String departmentPhoneNumber;
  /**
   * 승인 상태
   */
  @ValueConverter(FacilityApprovalStatusConverter.class)
  private FacilityApprovalStatus approvalStatus = FacilityApprovalStatus.PENDING;
  /**
   * 이미지 목록
   */
  private List<String> images;
  /**
   * 등록 회원 일련번호
   */
  private Long memberSeq;
  /**
   * 등록일시
   */
  @CreatedDate
  private LocalDateTime createdAt;
  /**
   * 수정일시
   */
  @LastModifiedDate
  private LocalDateTime updatedAt;

  public Facility(FacilityType type, String name, String location, String detailLocation, Decimal128 latitude,
      Decimal128 longitude, String information, List<String> images, Long memberSeq) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.images = images;
    this.memberSeq = memberSeq;
  }

  public void update(FacilityType type, String name, String location, String detailLocation, Decimal128 latitude,
      Decimal128 longitude, String information, List<String> images) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.images = images;

    // 시설물 수정 시, 승인 대기 상태로 변경
    this.approvalStatus = FacilityApprovalStatus.PENDING;
  }
}
