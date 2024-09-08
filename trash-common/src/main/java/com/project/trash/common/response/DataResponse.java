package com.project.trash.common.response;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.Getter;

/**
 * 단일 데이터 공통 응답 형식
 */
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DataResponse<T> extends SuccessResponse {

  private final T data;

  public DataResponse(T data) {
    this.data = data;
  }
}