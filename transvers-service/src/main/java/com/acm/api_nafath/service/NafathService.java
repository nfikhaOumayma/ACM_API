package com.acm.api_nafath.service;

import org.springframework.http.ResponseEntity;

import com.acm.utils.dtos.KeysDTO;
import com.acm.utils.dtos.MfaRequestDTO;
import com.acm.utils.dtos.NafathCallbackPayloadDTO;

/**
 * The {@code NafathService} interface defines methods for interacting with the Nafath API.
 *
 * @author nrmila
 */
public interface NafathService {

	/**
	 * Sends a request to the Nafath API.
	 *
	 * @param requestPayload The payload for the request.
	 * @param local The local parameter.
	 * @param requestId The request ID.
	 * @return The ResponseEntity containing the API response.
	 */
	ResponseEntity<?> sendRequest(MfaRequestDTO requestPayload, String local, String requestId);

	/**
	 * Retrieves JSON Web Keys (JWK) from the Nafath API.
	 *
	 * @return The ResponseEntity containing the API response.
	 */
	ResponseEntity<KeysDTO> retrieveJwk();

	/**
	 * Receives Nafath callbacks and verifies the JWT signature.
	 *
	 * @param callbackPayload The callback payload.
	 * @return The ResponseEntity containing the result of the callback handling.
	 */
	ResponseEntity<?> receiveCallback(NafathCallbackPayloadDTO callbackPayload);
}
