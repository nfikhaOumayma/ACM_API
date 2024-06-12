/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.security;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.provider.authentication.OAuth2AuthenticationDetails;
import org.springframework.stereotype.Component;

import feign.RequestInterceptor;
import feign.RequestTemplate;

/**
 * The {@link SecurityFeignClientInterceptor} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Component
public class SecurityFeignClientInterceptor implements RequestInterceptor {

	/** The Constant AUTHORIZATION_HEADER. */
	private static final String AUTHORIZATION_HEADER = "Authorization";

	/** The Constant BEARER_TOKEN_TYPE. */
	private static final String BEARER_TOKEN_TYPE = "bearer";

	/*
	 * (non-Javadoc)
	 * @see feign.RequestInterceptor#apply(feign.RequestTemplate)
	 */
	@Override
	public void apply(RequestTemplate template) {

		SecurityContext securityContext = SecurityContextHolder.getContext();
		Authentication authentication = securityContext.getAuthentication();
		if (authentication != null
				&& authentication.getDetails() instanceof OAuth2AuthenticationDetails) {
			OAuth2AuthenticationDetails details =
					(OAuth2AuthenticationDetails) authentication.getDetails();
			template.header(AUTHORIZATION_HEADER,
					String.format("%s %s", BEARER_TOKEN_TYPE, details.getTokenValue()));
		}
	}
}
