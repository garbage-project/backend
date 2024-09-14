package com.project.trash.facility.domain;

import com.project.trash.common.domain.BaseTimeEntity;

import org.springframework.data.annotation.CreatedBy;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 신고 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "REPORT")
public class Report extends BaseTimeEntity {

  @Id
  @Column(name = "RPT_ID", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long reportId;

  @Column(name = "RPT_CTT", nullable = false)
  private String content;

  @Column(name = "RPT_ANS")
  private String answer;

  @Column(name = "RPT_STT_YN", nullable = false)
  private Boolean status = Boolean.FALSE;

  @Column(name = "FCLTY_ID", nullable = false)
  private Long facilityId;

  @CreatedBy
  @Column(name = "MBR_ID", updatable = false, nullable = false)
  private Long memberId;

  public Report(String content, Long facilityId) {
    this.content = content;
    this.facilityId = facilityId;
  }

  public void update(String answer, Boolean status) {
    this.answer = answer;
    this.status = status;
  }
}
