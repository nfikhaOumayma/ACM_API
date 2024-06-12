/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.assertj.core.api.WithAssertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.function.Executable;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.mail.MailSendException;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.mail.javamail.MimeMessagePreparator;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.service.impl.MailServiceEngineImpl;
import com.acm.service.mailbuilder.MailContentBuilder;
import com.acm.utils.dtos.MailDTO;

/**
 * {@link MailServiceEngineImplTest} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
class MailServiceEngineImplTest implements WithAssertions {

	/** The mail service engine. */
	@InjectMocks
	private MailServiceEngineImpl mailServiceEngine;

	/** The mail sender. */
	@Spy
	private JavaMailSender mailSender;

	/** The mail content builder. */
	@Spy
	private MailContentBuilder mailContentBuilder;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
	}

	/**
	 * Should send mail.
	 */
	// @Test
	void shouldSendMail() {

		// given
		MailDTO mailDTO = new MailDTO("no-reply.acm.dev@acm.com", "haythem.benizid@dqlick.com",
				"Authentification Done", "Hello! sending mail to User.");
		MimeMessagePreparator messagePreparator = mimeMessage -> {
			MimeMessageHelper messageHelper = new MimeMessageHelper(mimeMessage);
			messageHelper.setFrom(mailDTO.getFrom());
			messageHelper.setTo(mailDTO.getTo());
			messageHelper.setSubject(mailDTO.getSubject());
			String content = mailContentBuilder.build(mailDTO);
			messageHelper.setText(content, true);
		};
		doNothing().when(mailSender).send(messagePreparator);
		// when
		mailServiceEngine.prepareAndSend(mailDTO);
		// then
		verify(mailSender, times(1)).send(any(MimeMessagePreparator.class));
	}

	/**
	 * Should throw MailException in case of pb when sending.
	 */
	@SuppressWarnings("unused")
	// @Test
	void shouldThrowCustomExceptionWhenNoDonneePerso() {

		// GIVEN
		MailDTO mailDTO = new MailDTO("no-reply.acm.dev@acm.com", "haythem.benizid@dqlick.com",
				"Authentification Done", "Hello! sending mail to User.");
		doThrow(new MailSendException("Test message")).when(mailSender)
				.send(any(MimeMessagePreparator.class));
		// WHEN
		Executable closureContainingCodeToTest = () -> mailServiceEngine.prepareAndSend(mailDTO);
		// THEN
		verify(mailSender, times(0)).send(any(MimeMessagePreparator.class));
	}

}
