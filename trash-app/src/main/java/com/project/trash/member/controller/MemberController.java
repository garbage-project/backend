package com.project.trash.member.controller;

import com.project.trash.common.response.DataResponse;
import com.project.trash.common.response.ListResponse;
import com.project.trash.common.response.SuccessResponse;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.facility.service.ReviewQueryService;
import com.project.trash.member.controller.validation.MemberValidator;
import com.project.trash.member.request.LoginRequest;
import com.project.trash.member.request.MemberDeleteRequest;
import com.project.trash.member.request.ReissueRequest;
import com.project.trash.member.response.AccessTokenInfoResponse;
import com.project.trash.member.response.MemberDetailResponse;
import com.project.trash.member.response.MyFacilityListResponse;
import com.project.trash.member.response.MyReviewListResponse;
import com.project.trash.member.response.LoginResponse;
import com.project.trash.member.service.MemberCommandService;
import com.project.trash.member.service.MemberQueryService;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
@Tag(name = "회원")
public class MemberController {

  private final MemberQueryService memberQueryService;
  private final MemberCommandService memberCommandService;
  private final FacilityQueryService facilityQueryService;
  private final ReviewQueryService reviewQueryService;

  @PostMapping("/delete")
  @Operation(summary = "회원 탈퇴",
      description = "로그인 회원의 정보를 삭제한다.")
  public SuccessResponse delete(@RequestBody MemberDeleteRequest param) {
    MemberValidator.validate(param);

    memberCommandService.delete(param);
    return new SuccessResponse();
  }

  @GetMapping("/my/facilities")
  @Operation(summary = "로그인 회원이 등록한 시설물 목록 조회",
      description = "회원이 등록한 시설물 목록을 조회한다.")
  public ListResponse<MyFacilityListResponse> getMyFacilities() {
    return new ListResponse<>(facilityQueryService.getList());
  }

  @GetMapping("/my")
  @Operation(summary = "로그인 회원 정보 조회",
      description = "로그인 회원의 정보를 조회한다.")
  public DataResponse<MemberDetailResponse> getMyInfo() {
    return new DataResponse<>(memberQueryService.getDetail());
  }

  @GetMapping("/my/reviews")
  @Operation(summary = "로그인 회원이 등록한 리뷰 목록 조회",
      description = "회원이 등록한 리뷰 목록을 조회한다.")
  public ListResponse<MyReviewListResponse> getMyReviews() {
    return new ListResponse<>(reviewQueryService.getList());
  }

  @PutMapping("/my/nickname")
  @Operation(summary = "로그인 회원 닉네임 수정",
      description = "로그인 회원의 닉네임을 수정한다.")
  public SuccessResponse putMyNickname(
      @Parameter(description = "수정할 닉네임", required = true)
      @RequestBody String nickname) {
    return new SuccessResponse();
  }

  @PostMapping("/login")
  @Operation(summary = "로그인",
      description = "로그인한다."
          + "\n[에러 코드]"
          + "\n- AUTH002 : OAuth 엑세스 토큰 정보가 유효하지 않습니다."
          + "\n- AUTH004 : 소셜 서비스의 회원 정보 조회를 실패했습니다.")
  public DataResponse<LoginResponse> postLogin(@RequestBody LoginRequest param) {
    MemberValidator.validate(param);

    return new DataResponse<>(memberCommandService.login(param));
  }

  @PostMapping("/logout")
  @Operation(summary = "로그아웃",
      description = "로그아웃한다."
          + "\n[에러 코드]"
          + "\n- AUTH000 : 토큰 정보가 존재하지 않습니다.")
  public SuccessResponse postLogout() {
    memberCommandService.logout();
    return new SuccessResponse();
  }

  @PostMapping("/reissue")
  @Operation(summary = "엑세스 토큰 재발급",
      description = "엑세스 토큰을 재발급한다."
          + "\n[에러 코드]"
          + "\n- MBR000 : 회원 정보가 존재하지 않습니다."
          + "\n- AUTH000 : 토큰 정보가 존재하지 않습니다."
          + "\n- AUTH001 : 토큰 정보가 유효하지 않습니다.")
  public DataResponse<AccessTokenInfoResponse> postReissue(@RequestBody ReissueRequest param) {
    MemberValidator.validate(param);

    return new DataResponse<>(memberCommandService.reissue(param));
  }
}
