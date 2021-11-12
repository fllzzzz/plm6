<template>
  <span class="table-cell-tag" :style="tagStyle">
    <el-tooltip :disabled="props.name.length > 4" :open-delay="300" :hide-after="10000" effect="light" :content="props.name" :placement="props.placement">
      <span style="cursor: pointer">{{ props.name.substr(0, 4) }}</span>
    </el-tooltip>
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'
const props = defineProps({
  name: {
    type: String,
    default: ''
  },
  color: {
    type: String,
    default: '#1682e6'
  },
  textColor: {
    type: String,
    default: '#FFFFFF'
  },
  placement: {
    type: String,
    default: 'left'
  }
})

const tagStyle = computed(() => {
  if (props.name) {
    return {
      'background-color': props.color,
      color: props.textColor
    }
  }
  return {}
})
</script>

<style lang="scss" scoped>
.table-cell-tag {
  background: #1682e6;
  transform:  rotate(-45deg);
  color: white;
  font-weight: 100;
  position: absolute;
  top: 5px;
  left: -20px;
  right: 0;
  width: 70px;
  height: 20px;
  font-size: 11px;
  display: flex;
  justify-content: center;
  align-items: center;
}
.table-cell-tag +span {
  // padding-left: 20px;
  margin-left: 15px;
  // margin-left: 10px;
}
</style>
