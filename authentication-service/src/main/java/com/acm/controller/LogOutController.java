
package com.acm.controller;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonLicenceVariable;
import com.google.common.base.Preconditions;

/**
 * {@link LogOutController} class used to control all the LogOut request.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@RestController
@RequestMapping("/users")
public class LogOutController {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(LogOutController.class);

	/** The token store. */
	@Autowired
	private TokenStore tokenStore;

	/**
	 * The method used to destroy the token.
	 * 
	 * @author HaythemBenizid
	 * @param request the request
	 * @return the map
	 */
	@GetMapping("/logOut")
	public Map<String, String> logOut(HttpServletRequest request) {

		Preconditions.checkNotNull(request, "Failed to find request.");
		String authHeader = request.getHeader("Authorization");
		Map<String, String> map = new LinkedHashMap<>();
		if (authHeader != null) {
			String tokenValue = authHeader.replaceFirst("(?i)" + "bearer", "").trim();
			logger.info("Token to remove value = {}", tokenValue);
			OAuth2AccessToken accessToken = tokenStore.readAccessToken(tokenValue);
			if (accessToken != null) {
				tokenStore.removeAccessToken(accessToken);
				logger.info("Token has been removed");
			}
			logger.info("Returnning log out url");
			map.put("URL_REDIRECTION_AFTER_LOGOUT", "/login");
		}
		if (CommonLicenceVariable.currentSumiltaniousUser != 0) {
			CommonLicenceVariable.currentSumiltaniousUser -= 1;
		}
		return map;
	}
}
