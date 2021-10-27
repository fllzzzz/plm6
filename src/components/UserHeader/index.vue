<template>
  <div class="avatar-container" :style="{ width: `${+props.avatarStyle['width'].split('px')[0] + 20}px`, height: props.avatarStyle['height'] }">
    <el-dropdown trigger="click">
      <el-tooltip class="item" effect="dark" :content="`当前用户：${user.name}`" :placement="placement">
        <div class="avatar-wrapper">
          <img class="user-avatar" src="@/assets/header.jpg" :style="props.avatarStyle" />
          <!-- <img class="user-avatar" :src="avatar+'?imageView2/1/w/80/h/80'"> -->
          <i class="el-icon-caret-bottom" />
        </div>
      </el-tooltip>
      <template #dropdown>
        <el-dropdown-menu class="user-dropdown">
          <router-link class="inlineBlock" to="/login">
            <el-dropdown-item> 菜单页 </el-dropdown-item>
          </router-link>
          <router-link class="inlineBlock" to="/personal/center">
            <el-dropdown-item> 个人中心 </el-dropdown-item>
          </router-link>
          <span v-if="showLayout" style="display: block" @click="show = true">
            <el-dropdown-item> 布局设置 </el-dropdown-item>
          </span>
          <el-dropdown-item> 消息通知 </el-dropdown-item>
          <el-dropdown-item divided>
            <span style="display: block" @click="logout">退出登录</span>
          </el-dropdown-item>
        </el-dropdown-menu>
      </template>
    </el-dropdown>
    <right-panel>
      <settings />
    </right-panel>
  </div>
</template>

<script setup>
// TODO: 配置访问地址/菜单页链接修改/ 头像目前写死
import { defineProps, computed } from 'vue'
import { useStore } from 'vuex'
import { useRoute, useRouter } from 'vue-router'
import { mapGetters } from '@/store/lib'
import RightPanel from '@comp/RightPanel/index.vue'
import settings from '@comp/RightPanel/settings/index.vue'

const props = defineProps({
  avatarStyle: {
    type: Object,
    default: () => {
      return {
        width: '40px',
        height: '40px',
        'border-radius': '50%'
      }
    }
  },
  placement: {
    type: String,
    default: 'left-end'
  },
  showLayout: {
    type: Boolean,
    default: true
  }
})

const store = useStore()
const route = useRoute()
const router = useRouter()

const { user } = mapGetters('user')

const show = computed({
  get() {
    return store.getters.showSettings
  },
  set(val) {
    store.dispatch('settings/changeSetting', new Map([['showSettings', val]]))
  }
})

// TODO: finally 修改
async function logout() {
  try {
    await store.dispatch('user/logout')
    router.push(`/login?redirect=${route.fullPath}`)
  } catch (error) {
    console.log(error)
  } finally {
    location.reload() // 为了重新实例化vue-router对象 避免bug
  }
}
</script>

<style lang="scss" scoped>
.avatar-container {
  // margin-right: 30px;
  .avatar-wrapper {
    // margin-top: 5px;
    position: relative;

    .user-avatar {
      cursor: pointer;
      width: 40px;
      height: 40px;
      border-radius: 10px;
    }

    .el-icon-caret-bottom {
      cursor: pointer;
      position: absolute;
      right: -20px;
      top: 25px;
      font-size: 12px;
    }
  }
}
</style>
