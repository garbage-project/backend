package com.project.trash.common.response;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.List;

import lombok.Getter;
import lombok.ToString;

/**
 * 단일 데이터와 목록 조회 공통 응답 형식
 */
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DataListResponse<T1, T2> extends SuccessResponse {
  private final T1 data;
  private final List<T2> list;
  private int count;

  public DataListResponse(T1 data, List<T2> list) {
    this.data = data;
    this.list = list;
    if (this.list != null && !this.list.isEmpty()) {
      this.count = this.list.size();
    }
  }
}
