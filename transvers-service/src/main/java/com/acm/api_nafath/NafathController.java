package com.acm.api_nafath;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_nafath.service.NafathService;
import com.acm.utils.dtos.MfaRequestDTO;
import com.acm.utils.dtos.NafathCallbackPayloadDTO;

/**
 * The Class NafathController.
 */
@RestController
@RequestMapping("/nafath-api")
public class NafathController {

	/** NAFATH service. */
	@Autowired
	private NafathService authService;

	/**
	 * Handles POST requests to send a Nafath request.
	 *
	 * @param local The desired localization (e.g., "ar").
	 * @param requestId The unique request identifier.
	 * @param requestPayload The request payload in the form of an MfaRequestDTO.
	 * @return A ResponseEntity containing the response to the request.
	 */
	@PostMapping("/request")
	public ResponseEntity<?> sendRequest(
			@RequestParam(name = "local", required = true, defaultValue = "ar") String local,
			@RequestParam(name = "requestId", required = true) String requestId,
			@RequestBody MfaRequestDTO requestPayload) {

		return authService.sendRequest(requestPayload, local, requestId);
	}

	/**
	 * Retrieve jwk.
	 *
	 * @return the response entity
	 */
	@GetMapping("/jwk")
	public ResponseEntity<?> retrieveJwk() {

		return authService.retrieveJwk();
	}

	/**
	 * Handles POST requests to receive Nafath callbacks and verify the JWT signature.
	 *
	 * @param callbackPayload The callback payload in the form of a NafathCallbackPayloadDTO.
	 * @return A ResponseEntity containing the verified claims from the JWT signature.
	 */
	@PostMapping("/webhook")
	public ResponseEntity<?> receiveCallback(
			@RequestBody NafathCallbackPayloadDTO callbackPayload) {

		return authService.receiveCallback(callbackPayload);
	}

}
