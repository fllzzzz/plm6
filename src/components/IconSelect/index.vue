<!-- @author zhengjie -->
<template>
  <div class="icon-body">
    <el-input v-model="name" style="position: relative;" clearable placeholder="请填写图标名称" @clear="filterIcons" @input="filterIcons">
      <template v-slot:suffix>
      <i class="el-icon-search el-input__icon" />
      </template>
    </el-input>
    <div class="icon-list">
      <div v-for="(item, index) in iconList.data" :key="index" @click="selectedIcon(item)">
        <svg-icon :icon-class="item" style="height: 30px;width: 16px;" />
        <span>{{ item }}</span>
      </div>
    </div>
  </div>
</template>

<script setup>
import icons from './require-icons'
import { defineExpose, defineEmits, ref, reactive } from 'vue'
const emit = defineEmits(['selected'])
const name = ref()
const iconList = reactive({
  data: icons
})

function filterIcons() {
  if (name.value) {
    iconList.data = iconList.data.filter(item => item.includes(name.value))
  } else {
    iconList.data = icons
  }
}
function selectedIcon(name) {
  emit('selected', name)
}
function reset() {
  name.value = ''
  iconList.data = icons
}

defineExpose({
  reset: reset
})
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  .icon-body {
    width: 100%;
    padding: 10px;
    .icon-list {
      height: 200px;
      overflow-y: scroll;
      div {
        height: 30px;
        line-height: 30px;
        margin-bottom: -5px;
        cursor: pointer;
        width: 33%;
        float: left;
      }
      span {
        display: inline-block;
        vertical-align: -0.15em;
        fill: currentColor;
        overflow: hidden;
      }
    }
  }
</style>
