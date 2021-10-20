<!-- 枚举类型通用单选按钮：单选按钮 -->
<template>
  <el-radio-group v-model="c_value" :size="size" @change="selectChange">
    <el-radio-button v-if="showAll" :label="undefined">全部</el-radio-button>
    <template v-for="item in options">
      <el-radio-button
        v-if="unshowVal.indexOf(item[c_props.value]) === -1"
        :key="item[c_props.key]"
        :label="item[c_props.value]"
        :disabled="disabledVal.indexOf(item[c_props.value]) > -1"
      >
        {{ item[c_props.label] }}
      </el-radio-button>
    </template>
  </el-radio-group>
</template>

<script>
const dictProps = { key: 'id', label: 'label', value: 'value' }
const enumProps = { key: 'K', label: 'L', value: 'V' }
const otherProps = { key: 'id', label: 'name', value: 'id' }

export default {
  name: 'CommonRadioButton',
  props: {
    value: [Number, String],
    size: {
      type: String,
      default: 'small'
    },
    options: {
      type: [Object, Array, Number],
      required: true
    },
    disabledVal: {
      type: Array,
      default: () => []
    },
    unshowVal: {
      type: Array,
      default: () => []
    },
    type: { // dict , enum, other
      type: String,
      default: 'dict'
    },
    showAll: {
      type: Boolean,
      default: false
    },
    // eslint-disable-next-line vue/require-default-prop
    props: {
      type: Object
    }
  },
  data() {
    return {
      c_value: null
    }
  },
  computed: {
    c_props() {
      if (!this.props) {
        if (this.type === 'dict') {
          return dictProps
        } else if (this.type === 'enum') {
          return enumProps
        } else {
          return otherProps
        }
      } else {
        return this.props
      }
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
