/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.websocket;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.MessageExceptionHandler;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.messaging.simp.annotation.SendToUser;
import org.springframework.stereotype.Controller;

/**
 * {@link NotificationSocketScheduler} class.
 *
 * @author idridi
 * @since 0.1.0
 */
@Controller
public class NotificationSocketScheduler {
	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(NotificationSocketScheduler.class);

	/** The simp messaging template. */
	@Autowired
	private SimpMessagingTemplate simpMessagingTemplate;

	/** The notifications services. */
	// @Autowired
	// private NotificationsServices notificationsServices;

	/** The Constant WS_MESSAGE_TRANSFER_DESTINATION. */
	private static final String WS_MESSAGE_TRANSFER_DESTINATION = "/topic/progress";

	/**
	 * Send message.
	 *
	 * @author idridi
	 * @param message the message
	 * @param sessionId the session id
	 * @return the string
	 * @throws Exception the exception
	 */
	@MessageMapping("/send")
	// @SendToUser("/topic/progress")
	public Long sendMessage(@Payload String message, @Header("simpSessionId") String sessionId)
			throws Exception {

		Long count = 0L;
		// count = notificationsServices.count(message);
		simpMessagingTemplate.convertAndSend("/queue/progress-" + sessionId, count);
		return count;
	}

	/**
	 * Handle exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the string
	 */
	@MessageExceptionHandler
	@SendToUser("/topic/errors")
	public String handleException(Throwable exception) {

		return exception.getMessage();
	}
}
