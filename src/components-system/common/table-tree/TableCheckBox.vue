<template>
  <label class="checkbox-label">
    <span :class="['checkbox-input', state ? 'checkbox-checked' : '']" @click="onClick">
      <input ref="checkbox" type="checkbox" :value="props.value">
      <span class="checkbox-inner" />
    </span>
    <span><slot /></span>
  </label>
</template>

<script setup>
import { defineEmits, defineProps, ref } from 'vue'
const props = defineProps({
  value: {
    type: [String, Number],
    default: undefined
  },
  checked: {
    type: Boolean,
    default: () => false
  }
})
const state = ref(props.checked)
const emit = defineEmits(['on-click'])
function onClick() {
  if (state.value) {
    state.value = false
  } else {
    state.value = true
  }
  emit('on-click', state.value, props.value)
}
</script>

<style scoped>
  .checkbox-label {font-family: "Chinese Quote", -apple-system, BlinkMacSystemFont, "Segoe UI", "PingFang SC", "Hiragino Sans GB", "Microsoft YaHei", "Helvetica Neue", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";font-size: 14px;font-variant: tabular-nums;color: rgba(0, 0, 0, 0.65);-webkit-box-sizing: border-box;box-sizing: border-box;margin: 0;padding: 0;list-style: none;line-height: unset;cursor: pointer;display: inline-block;}
  .checkbox-input {font-family: "Chinese Quote", -apple-system, BlinkMacSystemFont, "Segoe UI", "PingFang SC", "Hiragino Sans GB", "Microsoft YaHei", "Helvetica Neue", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";font-size: 14px;font-variant: tabular-nums;color: rgba(0, 0, 0, 0.65);-webkit-box-sizing: border-box;box-sizing: border-box;margin: 0;padding: 0;list-style: none;white-space: nowrap;cursor: pointer;outline: none;display: inline-block;line-height: 1;position: relative;vertical-align: middle;top: -0.09em;}
  .checkbox-inner {position: relative;top: 0;left: 0;display: block;width: 16px;height: 16px;border: 1px solid #d9d9d9;border-radius: 2px;background-color: #fff;-webkit-transition: all .3s;transition: all .3s;}
  .checkbox-inner:after {-webkit-transform: rotate(45deg) scale(0);transform: rotate(45deg) scale(0);position: absolute;left: 4.57142857px;top: 1.14285714px;display: table;width: 5.71428571px;height: 9.14285714px;border: 2px solid #fff;border-top: 0;border-left: 0;content: ' ';-webkit-transition: all 0.1s cubic-bezier(0.71, -0.46, 0.88, 0.6), opacity 0.1s;transition: all 0.1s cubic-bezier(0.71, -0.46, 0.88, 0.6), opacity 0.1s;opacity: 0;}
  .checkbox-checked:after {position: absolute;top: 0;left: 0;width: 100%;height: 100%;border-radius: 2px;border: 1px solid #1890ff;content: '';-webkit-animation: antCheckboxEffect 0.36s ease-in-out;animation: antCheckboxEffect 0.36s ease-in-out;-webkit-animation-fill-mode: both;animation-fill-mode: both;visibility: hidden;}
  .checkbox-checked .checkbox-inner:after {-webkit-transform: rotate(45deg) scale(1);transform: rotate(45deg) scale(1);position: absolute;display: table;border: 2px solid #fff;border-top: 0;border-left: 0;content: ' ';-webkit-transition: all 0.2s cubic-bezier(0.12, 0.4, 0.29, 1.46) 0.1s;transition: all 0.2s cubic-bezier(0.12, 0.4, 0.29, 1.46) 0.1s;opacity: 1;}
  .checkbox-input input[type="checkbox"] {position: absolute;left: 0;z-index: 1;cursor: pointer;opacity: 0;top: 0;bottom: 0;right: 0;width: 100%;height: 100%;margin: 4px 0 0;line-height: normal;-ms-touch-action: manipulation;touch-action: manipulation;-webkit-box-sizing: border-box;box-sizing: border-box;padding: 0;}
  .checkbox-label:hover .checkbox-inner {border: 1px solid #1890ff;}
  .checkbox-checked .checkbox-inner {background-color: #1890ff;border-color: #1890ff;}
</style>
