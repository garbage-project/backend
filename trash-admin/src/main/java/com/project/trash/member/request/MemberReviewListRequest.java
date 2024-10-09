package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MemberReviewListRequest extends PageRequest {

  @Parameter(description = "회원 ID", required = true, example = "1")
  private Long memberId;
}
