package com.project.trash.admin.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 관리자 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "ADMIN")
public class Admin {

  /**
   * ID
   */
  @Id
  @Column(name = "ADM_ID", nullable = false)
  private String id;
  /**
   * 비밀번호
   */
  @Column(name = "ADM_PWD", nullable = false)
  private String password;


}
