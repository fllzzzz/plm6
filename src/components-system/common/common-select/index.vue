<!-- 通用:下拉选择框 -->
<template>
  <el-select
    v-model="c_value"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    :filterable="filterable"
    :placeholder="placeholder"
    :class="textAlignClass"
    @change="selectChange"
    @blur="handleBlur"
  >
    <el-option
      v-if="showAll"
      :key="-1"
      :label="allLabelText"
      :value="allVal"
      :disabled="disabledVal.includes(allVal)"
    />
    <template v-for="item in options">
      <el-option
        v-if="unshowOptions.indexOf(item[c_props.key]) === -1"
        :key="item[c_props.key]"
        :label="item[c_props.label]"
        :value="item[c_props.value]"
        :disabled="disabledVal.includes(item[c_props.value])"
      >
        <slot :data="item" />
      </el-option>
    </template>
  </el-select>
</template>

<script>
const dictProps = { key: 'id', label: 'label', value: 'value' }
const enumProps = { key: 'K', label: 'L', value: 'V' }
const otherProps = { key: 'id', label: 'name', value: 'id' }

export default {
  name: 'CommonSelect',
  props: {
    // eslint-disable-next-line vue/require-default-prop
    value: {
      type: [Number, String, Array]
    },
    options: {
      type: [Array, Object],
      default: () => []
    },
    size: {
      type: String,
      default: 'small'
    },
    multiple: {
      type: Boolean,
      default: false
    },
    clearable: {
      type: Boolean,
      default: false
    },
    filterable: {
      type: Boolean,
      default: false
    },
    disabled: {
      type: Boolean,
      default: false
    },
    collapseTags: {
      type: Boolean,
      default: false
    },
    showAll: {
      type: Boolean,
      default: false
    },
    unshowOptions: { // value
      type: Array,
      default: () => []
    },
    placeholder: {
      type: String,
      default: '请选择'
    },
    textAlign: {
      type: String,
      default: 'left'
    },
    allLabelText: {
      type: String,
      default: '全部'
    },
    allVal: {
      type: [Number, String],
      default: undefined
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
      loading: false,
      c_value: undefined,
      defaultValue: undefined,
      c_props: null
    }
  },
  computed: {
    textAlignClass() {
      if (this.textAlign === 'center') {
        return 'alignCenter'
      }
      if (this.textAlign === 'left') {
        return 'alignLeft'
      }
      if (this.textAlign === 'right') {
        return 'alignRight'
      }
      return 'alignLeft'
    }
  },
  watch: {
    value(newVal) {
      this.c_value = newVal
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
    },
    handleBlur(event) {
      this.$emit('blur', event)
    },
    getOptions() {
      return this.options
    }
  }
}
</script>
<style lang="scss" scoped>
.alignCenter {
  ::v-deep(.el-input input){
    text-align: center;
  }
}
.alignLeft {
  ::v-deep(.el-input input){
    text-align: left;
  }
}
.alignRight {
  ::v-deep(.el-input input){
    text-align: right;
  }
}
</style>
