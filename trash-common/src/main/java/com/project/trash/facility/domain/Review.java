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
 * 리뷰 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "REVIEW")
public class Review extends BaseTimeEntity {

  @Id
  @Column(name = "RVW_ID", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long reviewId;

  @Column(name = "RVW_CTT", nullable = false)
  private String content;

  @Column(name = "FCLTY_ID", nullable = false)
  private Long facilityId;

  @CreatedBy
  @Column(name = "MBR_ID", updatable = false, nullable = false)
  private Long memberId;

  public Review(String content, Long facilityId) {
    this.content = content;
    this.facilityId = facilityId;
  }

  public void update(String content) {
    this.content = content;
  }
}
