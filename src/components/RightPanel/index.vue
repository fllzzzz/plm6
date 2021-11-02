<template>
  <div ref="rightPanel" :class="{show:show}" class="rightPanel-container">
    <div class="rightPanel-background" />
    <div class="rightPanel">
      <div v-show="show" class="handle-button" :style="{'top':buttonTop+'px','background-color':theme || '#1890ff'}" @click="show=!show">
        <i :class="show?'el-icon-close':'el-icon-setting'" />
      </div>
      <div class="rightPanel-items">
        <slot />
      </div>
    </div>
  </div>
</template>

<script setup>
import { addClass, removeClass } from '@/utils/element/class'
import { onMounted, defineProps, computed, watch, ref } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'

const store = useStore()

const props = defineProps({
  clickNotClose: {
    default: false,
    type: Boolean
  },
  buttonTop: {
    default: 250,
    type: Number
  }
})

// dom
const rightPanel = ref()

// ------------------ 计算属性 start --------------------------

// 主题
const { theme } = mapGetters('theme')

// 显示当前窗口
const show = computed({
  get() {
    return store.getters.showSettings
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['showSettings', val]]))
  }
})
// ------------------ 计算属性 end ----------------------------

// ------------------ 监听 start -----------------------------
// 监听
watch(
  show,
  (value) => {
    if (value && !props.clickNotClose) {
      addEventClick()
    }
    if (value) {
      addClass(document.body, 'showRightPanel')
    } else {
      removeClass(document.body, 'showRightPanel')
    }
  }
)
// ------------------ 监听 end -----------------------------

// ------------------ 生命周期 start ------------------------

onMounted(() => {
  // 当前打开了右侧页面风格设计时，并且刷新了浏览器，需要添加点击事件
  if (show.value) {
    addEventClick()
  }
  // insertToBody()
})
// ------------------ 生命周期 end ------------------------

function addEventClick() {
  window.addEventListener('click', closeSidebar, { passive: false })
}

function closeSidebar(evt) {
  const parent = evt.target.closest('.rightPanel')
  if (!parent) {
    show.value = false
    window.removeEventListener('click', this.closeSidebar)
  }
}

// 将dom插入到body中，并排在body子节点的第一位
// const insertToBody = () => {
//   const elx = rightPanel.value
//   const body = document.querySelector('body')
//   body.insertBefore(elx, body.firstChild)
// }
</script>

<style>
  .showRightPanel {
    overflow: hidden;
    position: relative;
    width: calc(100% - 15px);
  }
</style>

<style lang="scss" scoped>
  .rightPanel-background {
    position: fixed;
    top: 0;
    left: 0;
    opacity: 0;
    transition: opacity .3s cubic-bezier(.7, .3, .1, 1);
    background: rgba(0, 0, 0, .2);
    z-index: -1;
  }

  .rightPanel {
    width: 100%;
    max-width: 260px;
    height: 100vh;
    position: fixed;
    top: 0;
    right: 0;
    box-shadow: 0px 0px 15px 0px rgba(0, 0, 0, .05);
    transition: all .25s cubic-bezier(.7, .3, .1, 1);
    transform: translate(100%);
    background: #fff;
    z-index: 40000;
  }

  .show {
    transition: all .3s cubic-bezier(.7, .3, .1, 1);

    .rightPanel-background {
      z-index: 20000;
      opacity: 1;
      width: 100%;
      height: 100%;
    }

    .rightPanel {
      transform: translate(0);
    }
  }

  .handle-button {
    width: 48px;
    height: 48px;
    position: absolute;
    left: -48px;
    text-align: center;
    font-size: 24px;
    border-radius: 6px 0 0 6px !important;
    z-index: 0;
    pointer-events: auto;
    cursor: pointer;
    color: #fff;
    line-height: 48px;
    i {
      font-size: 24px;
      line-height: 48px;
    }
    opacity: 0.4;
  }
</style>
