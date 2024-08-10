package com.project.trash.auth.kakao.client;

import com.project.trash.auth.domain.OAuthMember;
import com.project.trash.auth.kakao.KakaoOAuthConfig;
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

/**
 * Kakao Api 요청
 */
@RequiredArgsConstructor
@Component
public class KakaoApiClient {

  private final KakaoOAuthConfig kakaoOAuthConfig;

  /**
   * 엑세스 토큰 정보 확인(검증)
   *
   * @return 소셜 ID
   */
  public String getAccessTokenInfo(String accessToken) {
    String resultText = WebClient.create(kakaoOAuthConfig.tokenInfoUri())
                                 .get()
                                 .header("Content-type", "application/x-www-form-urlencoded;charset=utf-8")
                                 .header(HttpHeaders.AUTHORIZATION,
                                     kakaoOAuthConfig.authorizationPrefix() + accessToken)
                                 .exchangeToMono(res -> res.bodyToMono(String.class))
                                 .block();

    return extractSocialId(resultText);
  }

  /**
   * 사용자 정보 가져오기
   */
  public OAuthMember getMemberInfo(String accessToken) {
    String resultText = WebClient.create(kakaoOAuthConfig.userInfoUri())
                                 .get()
                                 .header("Content-type", "application/x-www-form-urlencoded;charset=utf-8")
                                 .header(HttpHeaders.AUTHORIZATION,
                                     kakaoOAuthConfig.authorizationPrefix() + accessToken)
                                 .exchangeToMono(res -> res.bodyToMono(String.class))
                                 .block();

    return makeOAuthMember(resultText);
  }

  /**
   * 엑세스 토큰 발급
   */
  public String getToken(String authCode) {
    MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
    params.add("grant_type", "authorization_code");
    params.add("client_id", kakaoOAuthConfig.clientId());
    params.add("redirect_uri", kakaoOAuthConfig.redirectUri());
    params.add("code", authCode);
    params.add("client_secret", kakaoOAuthConfig.clientSecret());

    String resultText = WebClient.create(kakaoOAuthConfig.tokenUri())
                                 .post()
                                 .bodyValue(params)
                                 .header("Content-type", "application/x-www-form-urlencoded;charset=utf-8")
                                 .exchangeToMono(res -> res.bodyToMono(String.class))
                                 .block();

    return extractToken(resultText);
  }

  /**
   * 소셜 ID 추출
   */
  private String extractSocialId(String resultText) {
    try {
      JSONObject jsonObject = new JSONObject(resultText);
      return String.valueOf(jsonObject.getLong("id"));
    } catch (Exception e) {
      throw new ValidationException("auth.get_token_info_fail");
    }
  }

  /**
   * 엑세스 토큰 추출
   */
  private String extractToken(String resultText) {
    try {
      JSONObject jsonObject = new JSONObject(resultText);
      return jsonObject.getString("access_token");
    } catch (Exception e) {
      throw new ValidationException("auth.get_token_fail");
    }
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
      Long id = result.getLong("id");
      JSONObject kakaoAccount = result.getJSONObject("kakao_account");
      //      String name = kakaoAccount.getString("name");
      String email = kakaoAccount.getString("email");
      //      GenderType gender = kakaoAccount.getString("gender").equals("male") ? GenderType.MALE : GenderType.FEMALE;
      //      String birthday = kakaoAccount.getString("birthyear") + kakaoAccount.getString("birthday");
      return new OAuthMember(id.toString(), "테스트", email, GenderType.MALE, "20000216", SocialType.KAKAO);
    } catch (Exception e) {
      throw new ValidationException("auth.get_member_fail");
    }
  }
}
