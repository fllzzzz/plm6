<template>
  <div class="login-container">
    <div id="login-content" class="login-content">
      <el-form ref="loginForm" :model="loginForm" :rules="loginRules" class="login-form" auto-complete="off" label-position="left">
        <div class="title-container">
          <h3 class="title">MES管理系统</h3>
        </div>

        <el-form-item prop="username">
          <span class="svg-container">
            <svg-icon icon-class="user" />
          </span>
          <el-input
            ref="username"
            v-model="loginForm.username"
            placeholder="员工编号/手机号/邮箱"
            name="username"
            type="text"
            tabindex="1"
            auto-complete="off"
          />
        </el-form-item>

        <el-tooltip v-model="capsTooltip" content="大写锁定" placement="right" manual>
          <el-form-item prop="password">
            <span class="svg-container">
              <svg-icon icon-class="password" />
            </span>
            <el-input
              :key="passwordType"
              ref="password"
              v-model="loginForm.password"
              :type="passwordType"
              placeholder="密码"
              name="password"
              tabindex="2"
              auto-complete="off"
              @keyup="checkCapslock"
              @blur="capsTooltip = false"
              @keyup.enter="login"
            />
            <span class="show-pwd" @click="showPwd">
              <svg-icon :icon-class="passwordType === 'password' ? 'eye' : 'eye-open'" />
            </span>
          </el-form-item>
        </el-tooltip>
        <div class="forget-pwd-content">
          <!-- <el-checkbox v-model="loginForm.rememberMe">
            记住我
          </el-checkbox> -->
          <span />
          <div>
            <!-- TODO:忘记密码 -->
            <!-- <span @click="resetPwd">忘记密码？ &nbsp;|&nbsp;&nbsp;</span> -->
            <span />
            <span class="change-title" @click="clearRequestUrl">更换公司</span>
          </div>
        </div>
        <common-button :loading="loading" type="primary" style="width:100%;margin-bottom:30px;" @click="login">登录</common-button>
      </el-form>
    </div>
    <div v-show="showAuthent" id="authent" class="authent">
      <svg-icon icon-class="puff" />
      <p>{{ authentText }}</p>
    </div>
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import { codeWait } from '@/utils'
import { TweenMax } from 'gsap'

// TODO:代码重写
export default {
  name: 'LoginLoginComponent',
  data() {
    const validateUsername = (rule, value, callback) => {
      if (!value) {
        callback(new Error('请填写用户名'))
      } else {
        callback()
      }
    }
    const validatePass = (rule, value, callback) => {
      if (value.length < 6) {
        callback(new Error('密码不能小于6位'))
      } else {
        callback()
      }
    }
    return {
      waveColor: ['rgba(255, 255, 255, .3)', 'rgba(255, 255, 255, .5)'], // 波浪颜色
      waveHeight: [40, 30], // 波浪基础高度
      loginForm: { // 登录表单
        username: '', // 18005719019
        password: '',
        rememberMe: false
      },
      loginRules: { // 登录验证
        username: [{ required: true, trigger: 'blur', validator: validateUsername }],
        password: [{ required: true, trigger: 'blur', validator: validatePass }]
      },
      passwordType: 'password', // 密码框类型
      showAuthent: false, // 是否显示 认证框
      authentText: '认证中，请稍后', // 认证提示文字
      capsTooltip: false, // 是否显示 大写锁定提示
      loading: false, // 是否 正在提交登录表单
      showDialog: false, // 密码框类型
      redirect: undefined, // 跳转路径
      otherQuery: {}, // 携带参数
      loginTween: undefined,
      clientWidth: 0
    }
  },
  computed: {
    ...mapGetters([
      'token',
      'roles'
    ])
  },
  mounted() {
    this.clientWidth = document.documentElement.clientHeight
    window.addEventListener('resize', this.redraw, { passive: false })
  },
  methods: {
    // TODO: 忘记密码待开发
    resetPwd() {
      this.$message({ message: '待开发', type: 'warning' })
    },
    redraw() {
      this.clientWidth = document.documentElement.clientHeight
    },
    async clearRequestUrl() {
      await this.$store.dispatch('user/resetRequestUrl')
      this.$emit('show-reg-company')
    },
    /**
     * 判断是否大写锁定
     */
    checkCapslock({ shiftKey, key } = {}) {
      if (key && key.length === 1) {
        if (shiftKey && (key >= 'a' && key <= 'z') || !shiftKey && (key >= 'A' && key <= 'Z')) {
          this.capsTooltip = true
        } else {
          this.capsTooltip = false
        }
      }
      if (key === 'CapsLock' && this.capsTooltip === true) {
        this.capsTooltip = false
      }
    },
    /**
     * 显示密码
     */
    showPwd() {
      if (this.passwordType === 'password') {
        this.passwordType = ''
      } else {
        this.passwordType = 'password'
      }
      this.$nextTick(() => {
        this.$refs.password.focus()
      })
    },
    /**
     * 登录认证
     */
    async login() {
      this.$refs.loginForm.validate(async (valid) => {
        if (valid) {
          this.loading = true
          this.changeLoginBox(true)
          await this.changeAuthentBox(true)
          try {
            // 登录并在登录后获取用户信息
            await this.$store.dispatch('user/login', this.loginForm)
            this.authentText = '认证成功'
            await this.$store.dispatch('user/getInfo')
            await this.$store.dispatch('config/fetchConfigInfo')
          } catch (error) {
            this.authentText = '认证失败，请重新登录'
          } finally {
            this.loginResult()
          }
        } else {
          console.log('请填写正确的信息')
          return false
        }
      })
    },
    async loginResult() {
      // 等待1秒钟, 给用户看认证结果
      await codeWait(1000)
      this.loading = false
      this.changeAuthentBox(false)
      const logoinSuccess = Boolean(this.token && this.roles && this.roles.length > 0)
      if (logoinSuccess) {
        await this.removeLoginBox()
      } else {
        this.$store.dispatch('user/logout')
        await this.changeLoginBox(false)
      }

      this.$emit('login', { success: logoinSuccess })
    },
    async removeLoginBox() {
      const duration = 1000 // 动画持续时间
      this.loginTween = new TweenMax('#login-content', duration / 1000, {
        // x: -(this.clientWidth / 3),
        x: -500,
        alpha: 0
      })
      await codeWait(duration)
    },
    /**
     * 登录box动画
     * @param {boolean} show true:登录中;false:未登录
     */
    async changeLoginBox(show) {
      const duration = 1000 // 动画持续时间
      if (show) {
        // 登录动画 向左移动350px | 向x轴倾斜30° | 透明0.3
        this.loginTween = new TweenMax('#login-content', duration / 1000, {
          // x: -(this.clientWidth / 4),
          x: -200,
          rotateX: 30,
          alpha: 0.3
        })
      } else {
        // pause()
        this.loginTween.kill()
        // 回到初始状态
        TweenMax.to('#login-content', duration / 1000, {
          x: 0,
          rotateX: 0,
          alpha: 1
        })
      }
      await codeWait(duration)
    },
    /**
     * 认证box动画
     * @param {boolean} show true:认证中;false:未认证
     */
    async changeAuthentBox(show) {
      const duration = 1000 // 动画持续时间
      if (show) {
        // 初始化认证信息并显示
        this.authentText = '认证中，请稍后'
        this.showAuthent = true
        // TODO:重写
        // 登录动画 from-to：（左偏移400 | 透明0） -> （向右移动200px | 透明1）
        // TweenMax.fromTo('#authent', duration / 1000, { x: (this.clientWidth / 5), opacity: 0 }, { x: (this.clientWidth / 3), opacity: 1 })
        TweenMax.fromTo('#authent', duration / 1000, { x: 100, opacity: 0 }, { x: 300, opacity: 1 })
      } else {
        // 回到初始状态
        // TweenMax.fromTo('#authent', duration / 1000, { x: (this.clientWidth / 3), opacity: 1 }, { x: (this.clientWidth / 2), opacity: 0 })
        TweenMax.fromTo('#authent', duration / 1000, { x: 300, opacity: 1 }, { x: 500, opacity: 0 })
      }
      await codeWait(1000)
      this.showAuthent = show
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss">
$bg:#2d3a4b;
$light_gray:#eee;

/* reset element-ui css */
.login-container {
  .el-input {
    display: inline-block;
    height: 47px;
    width: 85%;
    input {
      background: transparent;
      border: 0px;
      -webkit-appearance: none;
      border-radius: 0px;
      padding: 12px 5px 12px 15px;
      color: $light_gray;
      height: 47px;
      &:-webkit-autofill {
        -webkit-box-shadow: 0 0 0 24px #2a466f inset !important;
        box-shadow: 0 0 0 24px #2a466f inset !important;
        -webkit-text-fill-color: #fff !important;
      }
    }
  }
  .el-form-item {
    border: 1px solid rgba(255, 255, 255, 0.1);
    background: #2a466f;
    border-radius: 5px;
    color: #454545;
  }
}

</style>

<style rel="stylesheet/scss" lang="scss" scoped>
$bg:transparent;
$dark_gray:#889aa4;
$light_gray:#eee;
.login-container {
  position: fixed;
  height: 100%;
  width: 100%;
  background-color: $bg;
  .authent {
    position: absolute;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    z-index: 5;
    left: 0;
    right: 0;
    width: 220px;
    height:100px;
    padding: 10px;
    margin: 270px auto;
    border-radius: 5px;
    letter-spacing:1px;
    background: rgba(0, 0, 0, 0.55);
    color:#fff;
    text-transform:uppercase;
  }
  .login-content {
    z-index: 5;
    position: absolute;
    left: 0;
    right: 0;
    width: 520px;
    padding: 35px 35px 15px 35px;
    margin: 120px auto;
  }
  .forget-pwd-content {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;
    margin:0 0 25px 0;
    padding: 0 5px;
    font-size: 14px;
    color: #fff;
    .el-checkbox {
      color: #fff
    }
    span {
      cursor: pointer;
    }
  }
  .tips {
    font-size: 14px;
    color: #fff;
    margin-bottom: 10px;
    span {
      &:first-of-type {
        margin-right: 16px;
      }
    }
  }
  .svg-container {
    padding: 6px 5px 6px 15px;
    color: $dark_gray;
    vertical-align: middle;
    width: 30px;
    display: inline-block;
    &_login {
      font-size: 20px;
    }
  }
  .title {
    font-size: 26px;
    font-weight: 400;
    color: $light_gray;
    margin: 0px auto 40px auto;
    text-align: center;
    font-weight: bold;
  }
  .show-pwd {
    position: absolute;
    right: 10px;
    top: 7px;
    font-size: 16px;
    color: $dark_gray;
    cursor: pointer;
    user-select: none;
  }
}
</style>
<style rel="stylesheet/scss" lang="scss">
.login-container{
  .el-checkbox__input.is-checked + .el-checkbox__label {
      color: #fff;
  }
}
</style>
