/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.oauth2server.service;

import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import com.acm.configuration.security.exception.UserNotActivatedException;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLicenceVariable;
import com.acm.repository.UserRepository;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmEnvironnement;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.User;
import com.acm.utils.repository.AcmEnvironnementRepository;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link UserAuthenticationManager} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Primary
@Component
public class UserAuthenticationManager implements AuthenticationManager {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(UserAuthenticationManager.class);

	/** The user repository. */
	@Autowired
	private UserRepository userRepository;

	@Autowired
	private AcmEnvironnementRepository acmEnvironnementRepository;

	/** The pwd failed attempts limit. */
	private Integer pwdFailedAttemptsLimit = null;

	/** The Constant USER_PWD_FAILED_ATTEMPTS_LIMIT. */
	private static final String USER_PWD_FAILED_ATTEMPTS_LIMIT = "USER_PWD_FAILED_ATTEMPTS_LIMIT";

	/** The Constant DEFAULT_PWD_FAILED_ATTEMPTS_LIMIT. */
	private static final Integer DEFAULT_PWD_FAILED_ATTEMPTS_LIMIT = 5;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** the BCryptPasswordEncoder. */
	@Autowired
	private BCryptPasswordEncoder bcryptPassword;

	/** The granted authority creater. */
	@Autowired
	private GrantedAuthorityCreater grantedAuthorityCreater;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.security.authentication.AuthenticationManager#authenticate(org.
	 * springframework.security.core.Authentication)
	 */
	@Override
	public Authentication authenticate(Authentication authentication) {

		Preconditions.checkNotNull(authentication, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Authenticate user by Login and check password");
		final String username = authentication.getName();
		final String password = authentication.getCredentials().toString();
		// get user details from DB by login
		User user = userRepository.findByUsernameIgnoreCase(username);
		if (ACMValidationUtils.isNullOrEmpty(user)) {
			throw new UsernameNotFoundException(CommonErrorCode.INCENTIVE_AUTHENTIFICATION_LOGIN);
		}
		else if (Boolean.TRUE.equals(checkUserCredentials(password, user))
				&& Boolean.TRUE.equals(user.getEnabled()) && checkResigningDate(user)) {
			// Successful login : Reset failed login attempts
			if (!user.getUsername().equals("acmbatch")) {

				CommonLicenceVariable.currentSumiltaniousUser += 1;
				logger.debug("current connected user  {}",
						CommonLicenceVariable.currentSumiltaniousUser);
			}

			resetFailedLoginAttempts(user);
			// check PWD expire date and mark as temporary to force PWD change
			checkPwdExpireDate(user);
			// check if user connected was NOT IB-USER
			Groupe ibGroupe = user.getGroupes().stream().filter(g -> g.getCode().equals("IB_GROUP"))
					.findAny().orElse(null);
			if (ACMValidationUtils.isNullOrEmpty(ibGroupe)) {

				UserDTO userDTO = mapper.map(user, UserDTO.class);

				return new UsernamePasswordAuthenticationToken(userDTO, user.getPassword(),
						grantedAuthorityCreater.findAuthoritys(user.getGroupes()));
			}
			else {
				throw new UserNotActivatedException(
						"User with Login : [" + username + "] Not authorized to Access.");
			}
		}
		else if (Boolean.TRUE.equals(checkUserCredentials(password, user))
				&& Boolean.FALSE.equals(checkResigningDate(user))) {
			throw new UserNotActivatedException(
					CommonErrorCode.INCENTIVE_AUTHENTIFICATION_RESIGNING_DATE);
		}

		else if (Boolean.TRUE.equals(checkUserCredentials(password, user))
				&& Boolean.FALSE.equals(user.getEnabled())) {
			throw new UserNotActivatedException(
					CommonErrorCode.INCENTIVE_AUTHENTIFICATION_USER_ENABLED);
		}
		else {
			updateFailedLoginAttempts(user);
			throw new UserNotActivatedException(
					CommonErrorCode.INCENTIVE_AUTHENTIFICATION_LOGIN_OR_PASSWORD);
		}

	}

	/**
	 * Update failed login attempts.
	 *
	 * @param user the user
	 */
	private void updateFailedLoginAttempts(User user) {

		Preconditions.checkNotNull(user);

		int failedCount =
				user.getFailedAttempts() != null ? user.getFailedAttempts().intValue() + 1 : 1;
		user.setFailedAttempts(failedCount);
		if (failedCount >= getPwdFailedAttemptsLimit()) {
			user.setEnabled(Boolean.FALSE);
		}
		userRepository.saveAndFlush(user);

	}

	/**
	 * Reset failed login attempts.
	 *
	 * @param user the user
	 */
	private void resetFailedLoginAttempts(User user) {

		Preconditions.checkNotNull(user);
		if (user.getFailedAttempts() == null || user.getFailedAttempts() > 0) {
			user.setFailedAttempts(0);
			userRepository.save(user);
		}
	}

	/**
	 * Check pwd expire date.
	 *
	 * @param user the user
	 */
	private void checkPwdExpireDate(User user) {

		Preconditions.checkNotNull(user);
		if (user.getPwdExpiryDate() != null && user.getPwdExpiryDate().before(new Date())) {
			user.setTemporaryPwd(Boolean.TRUE);
			userRepository.save(user);
		}
	}

	/**
	 * Check resigning date. If resigningDate is today then connect.If resigningDate before today
	 * then connect If resigningDate is null then connect. Else if reigningDate is before today then
	 * NOT CONNECT
	 * 
	 * @author ManelLamloum
	 * @param user the user
	 * @return the boolean
	 */
	private Boolean checkResigningDate(User user) {

		// Init result
		Boolean result = Boolean.FALSE;
		// check if resigning date exists for user OR resigning date is before today OR equals to
		// today ( resigningDate <= today )
		if ((ACMValidationUtils.isNullOrEmpty(user.getResigningDate()))
				|| (!ACMValidationUtils.isNullOrEmpty(user.getResigningDate())
						&& DateUtil.isDateOfInterestValid(new Date(), user.getResigningDate()))) {
			return Boolean.TRUE;
		}

		return result;
	}

	/**
	 * Check user credentials.
	 * 
	 * @author HaythemBenizid
	 * @param password the password
	 * @param user the user
	 * @return the boolean
	 */
	private Boolean checkUserCredentials(String password, User user) {

		Preconditions.checkNotNull(user);
		Preconditions.checkNotNull(user.getPassword());
		Preconditions.checkNotNull(password);
		return bcryptPassword.matches(password, user.getPassword());
	}

	/**
	 * Gets the pwd failed attempts limit. We use directly the AcmEnvironnement Repository as here
	 * security context is not ready yet to make feign call from Parametrage service.
	 *
	 * @return the pwd failed attempts limit
	 */
	public Integer getPwdFailedAttemptsLimit() {

		if (pwdFailedAttemptsLimit == null) {
			try {
				List<AcmEnvironnement> acmEnvironementValues =
						acmEnvironnementRepository.findByKey(USER_PWD_FAILED_ATTEMPTS_LIMIT);
				if (!ACMValidationUtils.isNullOrEmpty(acmEnvironementValues)
						&& ACMValidationUtils.isNumeric(acmEnvironementValues.get(0).getValue())) {
					pwdFailedAttemptsLimit =
							Integer.valueOf(acmEnvironementValues.get(0).getValue().trim());
				}
			}
			catch (Exception e) {
				logger.error("Cannont find User password field attempts limit !");
			}
			if (pwdFailedAttemptsLimit == null) {
				pwdFailedAttemptsLimit = DEFAULT_PWD_FAILED_ATTEMPTS_LIMIT; // Default Max Attempts
			}
		}
		return pwdFailedAttemptsLimit;
	}

}
