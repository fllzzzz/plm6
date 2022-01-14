<template>
  <div class="r_option-container">
    <span
      v-for="(item,index) in options"
      :key="index"
      class="r_option-drawer r_select-able"
      :style="{color:value !== undefined && value === item[c_props.value] ? activeColor : color, 'background-color':value !== undefined && value === item[c_props.value] ? buttonColor : 'unset'}"
      :class="{'r_active': value !== undefined && value === item[c_props.value] }"
      @click="handleSelectChange(item)"
    >
      <span class="r_title ellipsis-text">{{ item[c_props.label] }}</span>
    </span>
  </div>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'

const emit = defineEmits(['update:value', 'change'])

const props = defineProps({
  value: { // 选中的值
    type: null,
    default: undefined
  },
  required: { // 是否必须有值，为false，则再次点击选中的项，可取消选中
    type: Boolean,
    default: true
  },
  options: { // text,title,value
    type: [Array, Object],
    default: () => []
  },
  type: {
    type: String,
    default: 'enum'
  },
  color: {
    type: String,
    default: '#303133'
  },
  activeColor: {
    type: String,
    default: '#fff'
  },
  buttonColor: {
    type: String,
    default: '#409EFF'
  }
})

const c_props = ref()

const dictProps = { key: 'id', label: 'label', value: 'value' }
const enumProps = { key: 'K', label: 'L', value: 'V' }
const otherProps = { key: 'id', label: 'name', value: 'id' }

if (props.type === 'dict') {
  c_props.value = dictProps
} else if (props.type === 'enum') {
  c_props.value = enumProps
} else {
  c_props.value = otherProps
}

function handleSelectChange(option) {
  if (option[c_props.value.value] !== props.value) {
    handleEmit(option)
  } else if (!props.required) {
    handleEmit()
  }
}
function handleEmit(option) {
  emit('update:value', option ? option[c_props.value.value] : undefined)
  emit('change', option)
}
</script>

<style lang="scss" scoped>
.r_option-container {
    display: inline-flex;
    justify-content: space-between;
    align-items: center;
    user-select: none;
    .r_active {
      // border: 1px #303133 solid;
      padding: 5px 10px;
      box-sizing: border-box;
      border-radius: 15px;
      // background: #46a6ff;
      color:white;
      .option {
        border:none
      }
    }
}
.r_select-able {
  cursor: pointer;
}
.r_option-drawer {
    display: inline-flex;
    flex-direction: row;
    justify-content: flex-start;
    align-items: center;
    font-size: 14px;
    .option {
        width: 18px;
        height: 18px;
        border-radius: 50%;
        display: inline-block;
        margin-right: 10px;
        border: 1px solid #dcdfe6;
        flex: none;
    }
    .r_title {
      flex: auto;
      display: inline-block;
    }
}
.r_option-drawer +.r_option-drawer {
    margin-left: 10px;
}

</style>
