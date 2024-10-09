package com.project.trash.member.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MemberFacilityListRequest extends PageRequest {

  @Parameter(description = "회원 ID", example = "1", required = true)
  private String memberId;
}
