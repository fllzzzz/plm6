<template>
  <div id="app">
    <!-- <transition :name="transitionName">  -->
    <router-view />
    <!-- </transition> -->
  </div>
</template>

<script>
export default {
  name: 'App',
  data() {
    return {
      transitionName: ''
    }
  },
  watch: { // 使用watch 监听$router的变化
    $route(to, from) {
      // TODO: 如果此处不使用nextTick会导致，SwitchProject插件中的routeProjectType无法监听，暂时未找到原因
      this.$nextTick(() => {
        this.$store.dispatch('project/setRouteProjectMeta', to.meta)
      })
      if (to && to.path === '/login') {
        this.transitionName = null
      } else {
        this.transitionName = 'slide-left'
      }
    }
  }
}
</script>
<style lang="scss">
  @import './styles/index.scss'; // 全局自定义的css样式
  .slide-right-enter-active,
  .slide-right-leave-active,
  .slide-left-enter-active,
  .slide-left-leave-active {
    will-change: transform;
    transition: all 1000ms;
    position: absolute;
  }
  .slide-right-enter {
    opacity: 0;
    // transform: translate3d(-100%, 0, 0);
  }
  .slide-right-leave-active {
    opacity: 0;
    // transform: translate3d(100%, 0, 0);
  }
  .slide-left-enter {
    opacity: 0;
    // transform: translate3d(100%, 0, 0);
  }
  .slide-left-leave-active {
    opacity: 0;
    // transform: translate3d(-100%, 0, 0);
  }
</style>
