package com.acm.configuration.feignclient;

import java.io.IOException;
import java.io.InputStream;

import com.fasterxml.jackson.databind.ObjectMapper;

import feign.Response;
import feign.codec.ErrorDecoder;

public class CustomErrorDecoder implements ErrorDecoder {

	/*
	 * (non-Javadoc)
	 * @see feign.codec.ErrorDecoder#decode(java.lang.String, feign.Response)
	 */
	@Override
	public Exception decode(String methodKey, Response response) {

		String message = null;
		InputStream responseBodyIs = null;
		try {
			responseBodyIs = response.body().asInputStream();
			ObjectMapper mapper = new ObjectMapper();
			ExceptionMessage exceptionMessage =
					mapper.readValue(responseBodyIs, ExceptionMessage.class);

			message = exceptionMessage.getErrorMessage();
			return new Exception(message);
		}
		catch (IOException e) {

			e.printStackTrace();
		}
		finally {

			try {
				if (responseBodyIs != null) {
					responseBodyIs.close();
				}
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}

		switch (response.status()) {
			case 400:
				return new Exception("Bad Request error");
			case 404:
				return new Exception("Not Found error");
			default:
				return new Exception("Generic error");
		}
	}

}
