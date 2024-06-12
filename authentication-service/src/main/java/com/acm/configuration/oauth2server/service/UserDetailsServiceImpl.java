/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.oauth2server.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;

import com.acm.repository.UserRepository;
import com.acm.utils.models.User;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link UserDetailsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class UserDetailsServiceImpl implements UserDetailsService {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(UserDetailsServiceImpl.class);

	/** The user repository. */
	@Autowired
	private UserRepository userRepository;

	/** The granted authority creater. */
	@Autowired
	private GrantedAuthorityCreater grantedAuthorityCreater;

	/*
	 * (non-Javadoc)
	 * @see
	 * org.springframework.security.core.userdetails.UserDetailsService#loadUserByUsername(java.
	 * lang. String)
	 */
	@Override
	public UserDetails loadUserByUsername(String username) {

		logger.debug("Find user by userName {}", username);
		final User user = userRepository.findByUsernameIgnoreCase(username);
		return !ACMValidationUtils.isNullOrEmpty(user)
				? new org.springframework.security.core.userdetails.User(user.getUsername(),
						user.getPassword(), user.getEnabled(), true, true, true,
						grantedAuthorityCreater.findAuthoritys(user.getGroupes()))
				: null;
	}
}
