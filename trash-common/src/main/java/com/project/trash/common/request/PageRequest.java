package com.project.trash.common.request;

import com.fasterxml.jackson.annotation.JsonInclude;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
@NoArgsConstructor(access = lombok.AccessLevel.PUBLIC)
public class PageRequest {

  /**
   * 페이지 번호
   */
  @Parameter(description = "페이지 번호(기본 - 1)", example = "1")
  private int page = 1;
  /**
   * 페이지당 데이터 개수
   */
  @Parameter(description = "페이지당 데이터 개수(기본 - 20)", example = "10")
  private int size = 20;

  public PageRequest(int page, int size) {
    this.page = Math.max(page, 1);
    this.size = size < 1 ? 20 : size;
  }

  public int getOffset() {
    return getPage() > 0 ? (getPage() - 1) * getSize() : 0;
  }
}
