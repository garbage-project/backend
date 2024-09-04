package com.project.trash.token.domain;

import com.project.trash.common.domain.BaseTimeEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 토큰 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "TOKEN")
public class Token extends BaseTimeEntity {

  /**
   * 회원/관리자 고유 ID
   */
  @Id
  @Column(name = "MBR_ID", nullable = false)
  private String memberId;
  /**
   * 엑세스 토큰
   */
  @Column(name = "TKN_ACS", nullable = false)
  private String accessToken;
  /**
   * 리프레시 토큰
   */
  @Column(name = "TKN_RFRS", nullable = false)
  private String refreshToken;

  public Token(String memberId, String accessToken, String refreshToken) {
    this.memberId = memberId;
    this.accessToken = accessToken;
    this.refreshToken = refreshToken;
  }

  public void updateAccessToken(String accessToken) {
    this.accessToken = accessToken;
  }
}
