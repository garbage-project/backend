package com.project.trash.facility.domain;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;

import org.bson.types.Decimal128;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.mongodb.core.mapping.Document;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import lombok.Getter;

/**
 * 시설물 document
 */
@Getter
@Document
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
  private FacilityApprovalStatus approvalStatus = FacilityApprovalStatus.PENDING;
  
  @CreatedDate
  private LocalDateTime createdAt;
  @LastModifiedDate
  private LocalDateTime updatedAt;

  public Facility(FacilityType type, String location, String detailLocation, BigDecimal latitude, BigDecimal longitude,
      String information) {
    this.type = type;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = new Decimal128(latitude);
    this.longitude = new Decimal128(longitude);
    this.information = information;
  }
}
