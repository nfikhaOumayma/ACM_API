/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.utils.dtos.CalendarOutlookRequest;

/**
 * {@link CalendarSrvice} class.
 *
 * @author kouali
 * @since 0.1.0
 */
public interface CalendarSrvice {

	/**
	 * Send calendar invite.
	 *
	 * @param fromEmail the from email
	 * @param calendarRequest the calendar request
	 * @throws Exception the exception
	 */
	void sendCalendarInvite(String fromEmail, CalendarOutlookRequest calendarRequest)
			throws Exception;

}
