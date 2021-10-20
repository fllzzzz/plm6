<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group v-model="c_value" :size="size" :disabled="disabled" @change="selectChange">
    <el-radio
      v-for="item in options"
      :key="item[c_props.key]"
      :label="item[c_props.value]"
      :disabled="disabledVal.indexOf(item[c_props.value]) > -1"
    >
      {{ item[c_props.label] }}
    </el-radio>
  </el-radio-group>
</template>

<script>
const dictProps = { key: 'id', label: 'label', value: 'value' }
const enumProps = { key: 'K', label: 'L', value: 'V' }
const otherProps = { key: 'id', label: 'name', value: 'id' }

export default {
  name: 'CommonRadio',
  props: {
    value: {
      type: null,
      default: undefined
    },
    size: {
      type: String,
      default: 'small'
    },
    options: {
      type: [Object, Number],
      required: true
    },
    disabled: {
      type: Boolean,
      default: false
    },
    disabledVal: {
      type: Array,
      default: () => []
    },
    type: { // dict , enum, other
      type: String,
      default: 'dict'
    },
    // eslint-disable-next-line vue/require-default-prop
    props: {
      type: Object
    }
  },
  data() {
    return {
      c_value: null,
      c_props: null
    }
  },
  watch: {
    value(newVal) {
      // if (newVal || newVal === 0) {
      this.c_value = newVal
      // }
    }
  },
  created() {
    if (!this.props) {
      if (this.type === 'dict') {
        this.c_props = dictProps
      } else if (this.type === 'enum') {
        this.c_props = enumProps
      } else {
        this.c_props = otherProps
      }
    } else {
      this.c_props = this.props
    }
    this.c_value = this.value
  },
  methods: {
    selectChange(val) {
      this.$emit('update:value', val)
      this.$emit('change', val)
    }
  }
}
</script>
