/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;

import com.acm.utils.dtos.MessageDetailsDTO;

/**
 * The Interface SmsSenderService.
 */

public interface SmsSenderService {

	/**
	 * Login API abacus.
	 *
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	String loginAPIAbacus() throws IOException;

	/**
	 * Send sms.
	 *
	 * @param customerPhone the customer phone
	 * @param message the message
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	String sendSms(String customerPhone, String message) throws IOException;

	/**
	 * Save sms.
	 *
	 * @param messageDetailsDTO the message details DTO
	 * @return the message details DTO
	 */
	MessageDetailsDTO saveSms(MessageDetailsDTO messageDetailsDTO);

}
