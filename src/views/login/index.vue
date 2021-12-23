<template>
  <div class="login-menus-container">
    <div class="drag" />
    <div class="header-container">
      <div class="left-drawer">
        <logo />
      </div>
      <div id="rightDrawer" class="right-drawer">
        <head-switch-project v-if="!showLogin" class="projects-box" />

        <user-header v-show="!showLogin" :show-layout="true" />
      </div>
    </div>
    <template v-if="showLogin">
      <transition name="component-fade" mode="out-in">
        <component :is="currentView" style="z-index: 2" @reg-result="regCompanyCallback" @login="loginCallback" @show-reg-company="clearRequestUrl" />
        <!-- <reg-company v-if="showRegCompany" @reg-result="regCompanyCallback" style="z-index:2"/>
        <login v-else @login="loginCallback"
              style="z-index:2"></login> -->
      </transition>
    </template>
    <template v-if="!showLogin">
      <menus id="menus" style="z-index: 2" @replace="replaceCallback" />
    </template>
    <wave ref="wave" :wave-color="waveColor" :wave-duration="60" :wave-y-offset="waveYOffset" style="z-index: 1" />
    <div class="waveWrapperInner" />
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
// import { codeWait } from '@/utils'
import { TweenMax } from 'gsap'
import { getRequestUrl } from '@/utils/storage'
import Logo from '@comp/Logo'
import UserHeader from '@comp/UserHeader/index.vue'
import Wave from '@comp/Wave'
import HeadSwitchProject from '@/layout/components/SwitchProject/index.vue'
import Login from './login'
import Menus from './menu'
import RegCompany from './register-company'

// TODO: 整个登录需要重构
export default {
  name: 'LoginAndMenu',
  components: { Logo, UserHeader, Wave, Login, Menus, RegCompany, HeadSwitchProject },
  data() {
    return {
      showLogin: true, // 是否显示登录盒子
      showRegCompany: true, // 是否显示注册公司
      waveColor: ['rgba(255, 255, 255, .3)', 'rgba(255, 255, 255, .5)'], // 波浪颜色
      waveYOffset: 0.9 // y轴偏移
    }
  },
  computed: {
    ...mapGetters(['token']),
    currentView() {
      return this.showRegCompany ? 'reg-company' : 'login'
    }
  },
  created() {
    if (getRequestUrl()) {
      this.showRegCompany = false
    }
    if (this.token) {
      this.showLogin = false
    }
  },
  mounted() {
    if (this.token) {
      this.showMenu()
    }
  },
  activated() {
    // TODO: 是否有用，有待验证
    this.$refs.wave.play()
  },
  // beforeRouteLeave(to, from, next) {
  // this.$refs.wave.pause()
  // this.$refs.wave.kill()
  // next()
  // },
  beforeUnmount() {
    this.$refs.wave.kill()
  },
  methods: {
    /**
     * 登录状态回调
     */
    loginCallback(res) {
      if (res && res.success) {
        this.showMenu()
      }
    },
    regCompanyCallback(res) {
      if (res === true && getRequestUrl()) {
        this.showRegCompany = false
      }
    },
    replaceCallback(res) {
      if (res && this.$refs.wave) {
        this.$refs.wave.pause()
        // this.$refs.wave.kill()
      }
    },
    clearRequestUrl() {
      this.showRegCompany = true
    },
    showMenu() {
      this.showLogin = false
      this.$nextTick(() => {
        this.waveRise()
        this.showUserHeader()
        this.showMenuTween()
      })
    },
    /**
     * 显示用户
     */
    showUserHeader() {
      const duration = 1000 // 动画持续时间
      // 从上向下淡入
      // transform 会改变有 position:fixed 属性的后代元素的定位（变成根据有 transform 属性的祖先元素来定位，而不是根据浏览器来定位）
      this.menuTween = TweenMax.from('#rightDrawer', duration / 1000, { opacity: 0 })
      // this.menuTween = TweenMax.from('#rightDrawer', duration / 1000, { y: -100, opacity: 0 })
    },
    /**
     * 显示菜单
     */
    showMenuTween() {
      const duration = 2000 // 动画持续时间
      // 淡入
      this.menuTween = TweenMax.from('#menus', duration / 1000, { opacity: 0 })
      // this.menuTween = TweenMax.from('#menus', duration / 1000, { y: 1000, opacity: 0 })
    },
    getOtherQuery(query) {
      return Object.keys(query).reduce((acc, cur) => {
        if (cur !== 'redirect') {
          acc[cur] = query[cur]
        }
        return acc
      }, {})
    },
    /**
     * 波浪抬升
     */
    waveRise() {
      this.waveColor = ['rgba(58, 142, 230, .3)', 'rgba(58, 142, 230, .5)']
      this.waveYOffset = 0
      this.$nextTick(() => {
        this.$refs.wave.refresh()
      })
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
$bg: #2d3a4b;
$dark_gray: #889aa4;
$light_gray: #eee;
.login-menus-container {
  position: fixed;
  height: 100%;
  width: 100%;
  background-color: $bg;
  .header-container {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    width: 100%;
    margin: 20px auto;
    padding: 40px;
    box-sizing: border-box;
    z-index: 5;
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;

    .right-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-end;
      align-items: center;
    }

    .change-title {
      color: #ffffff;
      cursor: pointer;
    }
    .projects-box {
      margin-right: 10px;
      color: white;
      ::v-deep(.el-input input) {
        color: white;
      }
    }
  }
  .waveWrapperInner {
    position: absolute;
    width: 100%;
    overflow: hidden;
    height: 100%;
    bottom: -1px;
    background-image: linear-gradient(to top, #409eff 20%, #27273c 100%);
  }
}
</style>
