package com.project.trash.facility.domain;

import com.project.trash.common.domain.BaseTimeEntity;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;

import java.math.BigDecimal;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 시설물 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "FACILITY")
public class Facility extends BaseTimeEntity {

  /**
   * 시설물 일련번호
   */
  @Id
  @Column(name = "FCLTY_SEQ", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long facilitySeq;
  /**
   * 시설물명
   */
  @Column(name = "FCLTY_NM", nullable = false)
  private String name;
  /**
   * 시설물 종류
   */
  @Convert(converter = FacilityType.TypeCodeConverter.class)
  @Column(name = "FCLTY_TYP", nullable = false)
  private FacilityType type;
  /**
   * 위치
   */
  @Column(name = "FCLTY_LCTN", nullable = false)
  private String location;
  /**
   * 상세 위치
   */
  @Column(name = "FCLTY_DTL_LCTN", nullable = false)
  private String detailLocation;
  /**
   * 위도
   */
  @Column(name = "FCLTY_LTTD", nullable = false)
  private BigDecimal latitude;
  /**
   * 경도
   */
  @Column(name = "FCLTY_LNGT", nullable = false)
  private BigDecimal longitude;
  /**
   * 정보
   */
  @Column(name = "FCLTY_INFO", nullable = false)
  private String information;
  /**
   * 관리 부서명
   */
  @Column(name = "FCLTY_DPR_NM")
  private String department;
  /**
   * 관리 부서 전화번호
   */
  @Column(name = "FCLTY_DPR_TLPH_NMBR")
  private String departmentPhoneNumber;
  /**
   * 승인 상태
   */
  @Convert(converter = FacilityApprovalStatus.TypeCodeConverter.class)
  @Column(name = "FCLTY_APRV_STA", nullable = false)
  private FacilityApprovalStatus approvalStatus = FacilityApprovalStatus.PENDING;
  /**
   * 회원 ID
   */
  @Column(name = "MBR_ID", updatable = false)
  private String memberId;

  public Facility(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String memberId) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.memberId = memberId;
  }

  public Facility(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String department, String departmentPhoneNumber,
      FacilityApprovalStatus approvalStatus, String memberId) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.department = department;
    this.departmentPhoneNumber = departmentPhoneNumber;
    this.approvalStatus = approvalStatus;
    this.memberId = memberId;
  }

  public void update(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;

    // 시설물 수정 시, 승인 대기 상태로 변경
    this.approvalStatus = FacilityApprovalStatus.PENDING;
  }

  public void update(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String department, String departmentPhoneNumber,
      FacilityApprovalStatus approvalStatus) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.department = department;
    this.departmentPhoneNumber = departmentPhoneNumber;
    this.approvalStatus = approvalStatus;
  }
}
