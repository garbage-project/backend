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

  /**
   * 신고 일련번호
   */
  @Id
  @Column(name = "RPT_SEQ", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long reportSeq;
  /**
   * 신고 내용
   */
  @Column(name = "RPT_CTT", nullable = false)
  private String content;
  /**
   * 답변
   */
  @Column(name = "RPT_ANS")
  private String answer;
  /**
   * 처리상태
   */
  @Column(name = "RPT_STT_YN", nullable = false)
  private Boolean status = Boolean.FALSE;
  /**
   * 시설물 ID
   */
  @Column(name = "FCLTY_ID", nullable = false)
  private String facilityId;
  /**
   * 회원 일련번호
   */
  @CreatedBy
  @Column(name = "MBR_SEQ", updatable = false, nullable = false)
  private Long memberSeq;

  public Report(String content, String facilityId) {
    this.content = content;
    this.facilityId = facilityId;
  }

  public void update(String answer, Boolean status) {
    this.answer = answer;
    this.status = status;
  }
}
