<!-- 表格类型:下拉选择框 -->
<template>
  <div>
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
    >
      <el-option
        v-if="showAll"
        :key="-1"
        :label="allLabelText"
        :value="undefined"
      />
      <template v-for="item in tableTypeOptions">
        <el-option
          v-if="unshowOptions.indexOf(item[props.key]) === -1"
          :key="item[props.key]"
          :label="item[props.label]"
          :value="item[props.value]"
        />
      </template>
    </el-select>
  </div>

</template>

<script>
import enumOperate, { moduleTypeEnum, tableTypeEnum } from '@/utils/print/table-type'
const tableTypeArr = enumOperate.toArr(tableTypeEnum)

export default {
  props: {
    // eslint-disable-next-line vue/require-default-prop
    value: {
      type: [Number, String, Array]
    },
    options: {
      type: [Array, Object],
      default: () => []
    },
    moduleType: {
      type: [Number, String],
      default: undefined
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
    default: {
      type: Boolean,
      default: false
    },
    unshowOptions: { // value
      type: Array,
      default: () => []
    },
    placeholder: {
      type: String,
      default: '请选择表格'
    },
    allLabelText: {
      type: String,
      default: '模块下所有表格模板'
    }
  },
  data() {
    return {
      loading: false,
      c_value: undefined,
      defaultValue: undefined,
      props: { key: 'K', label: 'L', value: 'K' },
      moduleTypeEnum,
      tableTypeEnum,
      tableTypeArr
    }
  },
  computed: {
    tableTypeOptions() {
      if (this.moduleType) {
        return tableTypeArr.filter(item => {
          return item.T === this.moduleType
        })
      } else {
        return tableTypeArr
      }
    }
  },
  watch: {
    value: {
      handler(val) {
        this.c_value = val
      }, immediate: true
    },
    tableTypeOptions: {
      handler() {
        if (this.tableTypeOptions && this.tableTypeOptions.length) {
          const exist = this.tableTypeOptions.some(t => t.V === this.c_value)
          if (!exist) {
            if (this.default) {
              this.c_value = this.tableTypeOptions[0].V
            } else {
              this.c_value = undefined
            }
          }
        } else {
          this.c_value = undefined
        }
      }, immediate: true
    },
    c_value: {
      handler(val) {
        this.selectChange(val)
      }
    }
  },
  created() {
    this.c_value = this.value
  },
  methods: {
    selectChange(val) {
      this.$emit('update:value', val)
      this.$emit('change', val)
    },
    getOptions() {
      return this.options
    }
  }
}
</script>
