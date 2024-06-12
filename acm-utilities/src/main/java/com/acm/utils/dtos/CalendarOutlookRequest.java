/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * The Class CalendarOutlookRequest.
 */
public final class CalendarOutlookRequest {

	/** The uid. */
	private String uid = UUID.randomUUID().toString();

	/** The to email. */
	private String toEmail;

	/** The subject. */
	private String subject;

	/** The body. */
	private String body;

	/** The meeting start time. */
	private LocalDateTime meetingStartTime;

	/** The meeting end time. */
	private LocalDateTime meetingEndTime;

	/** The location. */
	private String location;

	/**
	 * Instantiates a new calendar outlook request.
	 *
	 * @param builder the builder
	 */
	private CalendarOutlookRequest(Builder builder) {

		toEmail = builder.toEmail;
		subject = builder.subject;
		body = builder.body;
		meetingStartTime = builder.meetingStartTime;
		meetingEndTime = builder.meetingEndTime;
		location = builder.location;

	}

	/**
	 * Gets the uid.
	 *
	 * @return the uid
	 */
	public String getUid() {

		return uid;
	}

	/**
	 * Gets the to email.
	 *
	 * @return the to email
	 */
	public String getToEmail() {

		return toEmail;
	}

	/**
	 * Gets the subject.
	 *
	 * @return the subject
	 */
	public String getSubject() {

		return subject;
	}

	/**
	 * Gets the body.
	 *
	 * @return the body
	 */
	public String getBody() {

		return body;
	}

	/**
	 * Gets the meeting start time.
	 *
	 * @return the meeting start time
	 */
	public LocalDateTime getMeetingStartTime() {

		return meetingStartTime;
	}

	/**
	 * Gets the meeting end time.
	 *
	 * @return the meeting end time
	 */
	public LocalDateTime getMeetingEndTime() {

		return meetingEndTime;
	}

	/**
	 * Gets the location.
	 *
	 * @return the location
	 */
	public String getLocation() {

		return location;
	}

	/**
	 * Sets the location.
	 *
	 * @param location the location to set
	 */
	public void setLocation(String location) {

		this.location = location;
	}

	/**
	 * The Class Builder.
	 */
	public static final class Builder {

		/** The to email. */
		private String toEmail;

		/** The subject. */
		private String subject;

		/** The body. */
		private String body;

		/** The meeting start time. */
		private LocalDateTime meetingStartTime;

		/** The meeting end time. */
		private LocalDateTime meetingEndTime;

		/** The location. */
		private String location;

		/**
		 * Instantiates a new builder.
		 */
		public Builder() {

			// test
		}

		/**
		 * With to email.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withToEmail(String val) {

			toEmail = val;
			return this;
		}

		/**
		 * With subject.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withSubject(String val) {

			subject = val;
			return this;
		}

		/**
		 * With body.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withBody(String val) {

			body = val;
			return this;
		}

		/**
		 * With meeting start time.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withMeetingStartTime(LocalDateTime val) {

			meetingStartTime = val;
			return this;
		}

		/**
		 * With meeting end time.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withMeetingEndTime(LocalDateTime val) {

			meetingEndTime = val;
			return this;
		}

		/**
		 * With location.
		 *
		 * @param val the val
		 * @return the builder
		 */
		public Builder withLocation(String val) {

			location = val;
			return this;
		}

		/**
		 * Builds the.
		 *
		 * @return the calendar outlook request
		 */
		public CalendarOutlookRequest build() {

			return new CalendarOutlookRequest(this);
		}
	}
}
