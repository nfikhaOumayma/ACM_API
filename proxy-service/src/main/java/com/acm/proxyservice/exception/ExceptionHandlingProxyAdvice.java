/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice.exception;

import java.io.Serializable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.client.HttpClientErrorException;

import feign.FeignException;

/**
 * {@link ExceptionHandlingProxyAdvice} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@RestControllerAdvice
public class ExceptionHandlingProxyAdvice {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(ExceptionHandlingProxyAdvice.class);

	/**
	 * Resource is null using NullPointerException class.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NullPointerException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceIsNull(NullPointerException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_NULL_POINTER_EXCEPTION");
		response.setErrorMessage("Internal error has been occurred, contact administrator.");

		logger.error("PROXY - Fire resource Is Null Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Resource is illegal argument exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IllegalArgumentException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceIllegalArgumentException(
			IllegalArgumentException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_ILLEGAL_ARGUMENT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire resource Is IllegalArgument Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Data access exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(DataAccessException.class)
	public ResponseEntity<ExceptionResponseMessage> dataAccessException(
			DataAccessException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_DATA_ACCESS_EXCEPTION");
		response.setErrorMessage(
				"Cannot execute this query as it might involve data access problem.");
		logger.error("PROXY - Fire data Access Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * No such method exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NoSuchMethodException.class)
	public ResponseEntity<ExceptionResponseMessage> noSuchMethodException(
			NoSuchMethodException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_NO_SUCH_METHOD_EXCEPTION");
		response.setErrorMessage("NoSuchMethodException");

		logger.error("PROXY - Fire No Such Method Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Http client error exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(HttpClientErrorException.class)
	public ResponseEntity<ExceptionResponseMessage> httpClientErrorException(
			HttpClientErrorException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_HTTP_CLIENT_ERROR_EXCEPTION");
		response.setErrorMessage(
				"httpClientErrorException : " + exception.getResponseBodyAsString());

		logger.error("PROXY - Fire Http Client Error Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Illegal state exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IllegalStateException.class)
	public ResponseEntity<ExceptionResponseMessage> illegalStateException(
			IllegalStateException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_ILLEGAL_STATE_EXCEPTION");
		response.setErrorMessage("illegalStateException : " + exception.getMessage());

		logger.error("PROXY - Fire illegal State Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (com.netflix.client) Client Exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(com.netflix.client.ClientException.class)
	public ResponseEntity<ExceptionResponseMessage> clientException(
			com.netflix.client.ClientException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_CLIENT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire com.netflix.client.ClientException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (java.net.ConnectException) Connect Exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(java.net.ConnectException.class)
	public ResponseEntity<ExceptionResponseMessage> connectException(
			java.net.ConnectException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_CONNECT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire java.net.ConnectException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (com.netflix.zuul.exception) Zuul Exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(com.netflix.zuul.exception.ZuulException.class)
	public ResponseEntity<ExceptionResponseMessage> zuulException(
			com.netflix.zuul.exception.ZuulException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_ZUUL_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire com.netflix.zuul.exception.ZuulException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Feign exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the feign exception
	 * @return the response entity
	 */
	@ExceptionHandler(FeignException.class)
	public ResponseEntity<ExceptionResponseMessage> feignExceptionHandler(
			FeignException exception) {

		logger.error("PROXY - Feign exception thrown.");
		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_FEIGN_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error(exception.getMessage(), exception);
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Socket timeout exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(java.net.SocketTimeoutException.class)
	public ResponseEntity<ExceptionResponseMessage> socketTimeoutException(
			java.net.SocketTimeoutException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_SOCKETTIMEOUT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire java.net.SocketTimeoutException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (com.netflix.discovery.shared.transport.TransportException)Transport exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(com.netflix.discovery.shared.transport.TransportException.class)
	public ResponseEntity<ExceptionResponseMessage> transportException(
			com.netflix.discovery.shared.transport.TransportException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_TRANSPORT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire com.netflix.discovery.shared.transport.TransportException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * {@link ExceptionResponseMessage} class.
	 *
	 * @author HaythemBenizid
	 * @since 1.0.2
	 */
	public static class ExceptionResponseMessage implements Serializable {

		/** The Constant serialVersionUID. */
		private static final long serialVersionUID = -9212767840435385398L;

		/** The error code. */
		private String errorCode;

		/** The error message. */
		private String errorMessage;

		/**
		 * Instantiates a new exception response message.
		 */
		public ExceptionResponseMessage() {

			/*
			 * 
			 */
		}

		/**
		 * Instantiates a new response msg.
		 *
		 * @param errorMessage the error message
		 */
		public ExceptionResponseMessage(String errorMessage) {

			this.errorMessage = errorMessage;
		}

		/**
		 * Instantiates a new exception response message.
		 *
		 * @param errorCode the error code
		 * @param errorMessage the error message
		 */
		public ExceptionResponseMessage(String errorCode, String errorMessage) {

			this.errorCode = errorCode;
			this.errorMessage = errorMessage;
		}

		/**
		 * Gets the error code.
		 *
		 * @return the errorCode
		 */
		public String getErrorCode() {

			return errorCode;
		}

		/**
		 * Sets the error code.
		 *
		 * @param errorCode the errorCode to set
		 */
		public void setErrorCode(String errorCode) {

			this.errorCode = errorCode;
		}

		/**
		 * Gets the error message.
		 *
		 * @return the errorMessage
		 */
		public String getErrorMessage() {

			return errorMessage;
		}

		/**
		 * Sets the error message.
		 *
		 * @param errorMessage the errorMessage to set
		 */
		public void setErrorMessage(String errorMessage) {

			this.errorMessage = errorMessage;
		}
	}
}
