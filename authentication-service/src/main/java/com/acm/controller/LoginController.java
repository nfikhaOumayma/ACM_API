
package com.acm.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonFunctions;
import com.acm.utils.dtos.LoginDTO;
import com.google.common.base.Preconditions;

/**
 * {@link LoginController} class used to control the Login request.
 *
 * @author yassine
 * @since 1.12.0
 */
@RestController
@RequestMapping("/login")
public class LoginController {

	/** The url serveur authentification. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/**
	 * The method used to destroy the token.
	 *
	 * @author yassine
	 * @param loginRequest the login request
	 * @return the map
	 */
	@PostMapping("/authenticate")
	public ResponseEntity<String> login(@RequestBody LoginDTO loginRequest) {

		Preconditions.checkNotNull(loginRequest, "Failed to find request.");

		return CommonFunctions.generateToken(urlServeurAuthentification, loginRequest.getLogin(),
				loginRequest.getPassword());
	}

}
