package com.project.trash.auth.naver.client;

import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.naver.NaverOAuthConfig;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.member.domain.enums.GenderType;
import com.project.trash.member.domain.enums.SocialType;

import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.WebClient;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_OAUTH_GET_ACCESS_TOKEN_FAIL;
import static com.project.trash.common.domain.resultcode.AuthResultCode.AUTH_OAUTH_GET_MEMBER_FAIL;

/**
 * Naver Api 요청
 */
@RequiredArgsConstructor
@Component
public class NaverApiClient {

  private final NaverOAuthConfig naverOAuthConfig;

  /**
   * 사용자 정보 조회
   */
  public OAuthMember getMemberInfo(String accessToken) {
    return makeOAuthMember(fetchMemberInfo(accessToken));
  }

  /**
   * 소셜 ID 조회
   */
  public String getSocialId(String accessToken) {
    try {
      JSONObject result = new JSONObject(fetchMemberInfo(accessToken));
      return result.getJSONObject("response").getString("id");
    } catch (Exception e) {
      throw new ValidationException(AUTH_OAUTH_GET_MEMBER_FAIL);
    }
  }

  /**
   * 엑세스 토큰 발급
   */
  public String getToken(String authCode) {
    MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
    params.add("grant_type", "authorization_code");
    params.add("client_id", naverOAuthConfig.clientId());
    params.add("redirect_uri", naverOAuthConfig.redirectUri());
    params.add("code", authCode);
    params.add("client_secret", naverOAuthConfig.clientSecret());

    String resultText = WebClient.create(naverOAuthConfig.tokenUri())
                                 .post()
                                 .bodyValue(params)
                                 .header("Content-type", "application/x-www-form-urlencoded;charset=utf-8")
                                 .exchangeToMono(res -> res.bodyToMono(String.class))
                                 .block();

    return extractToken(resultText);
  }

  /**
   * 엑세스 토큰 추출
   */
  private String extractToken(String resultText) {
    try {
      JSONObject jsonObject = new JSONObject(resultText);
      return jsonObject.getString("access_token");
    } catch (Exception e) {
      throw new ValidationException(AUTH_OAUTH_GET_ACCESS_TOKEN_FAIL);
    }
  }

  /**
   * 사용자 정보 조회 API 요청
   */
  private String fetchMemberInfo(String accessToken) {
    return WebClient.create(naverOAuthConfig.userInfoUri())
                    .get()
                    .header("Content-type", "application/x-www-form-urlencoded;charset=utf-8")
                    .header(HttpHeaders.AUTHORIZATION, naverOAuthConfig.authorizationPrefix() + accessToken)
                    .exchangeToMono(res -> res.bodyToMono(String.class))
                    .block();
  }

  /**
   * json 문자열 파싱 - 회원 정보
   *
   * @param resultText 응답 문자열
   * @return 회원 정보
   */
  private OAuthMember makeOAuthMember(String resultText) {
    try {
      JSONObject result = new JSONObject(resultText);
      JSONObject response = result.getJSONObject("response");
      String id = response.getString("id");
      String name = response.getString("name");
      String email = response.getString("email");
      String gender = response.getString("gender");
      GenderType genderType = gender.equals("U") ? GenderType.NONE : GenderType.fromCode(gender);
      String birthday = response.getString("birthyear") + response.getString("birthday").replace("-", "");
      return new OAuthMember(id, name, email, genderType, birthday, SocialType.NAVER);
    } catch (Exception e) {
      throw new ValidationException(AUTH_OAUTH_GET_MEMBER_FAIL);
    }
  }
}
