<!-- 单体:下拉选择框 -->
<template>
  <el-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    :no-data-text="projectId ? '无数据': '未选择项目'"
  >
    <el-option
      v-if="showAll"
      label="全部单体"
      :value="undefined"
    />
    <el-option
      v-for="item in options"
      :key="item.value"
      :label="item.label"
      :value="item.value"
    />
  </el-select>
</template>

<script>
import { getMonomersAllSimple as getAll } from '@/api/mes-plan/common'
import { monomerAreaClassifyEnum } from '@/utils/enum/index'
export default {
  props: {
    // eslint-disable-next-line vue/require-default-prop
    projectId: {
      type: [Number, String]
    },
    areaClassify: {
      type: [Number],
      default: monomerAreaClassifyEnum.HAS_AREA.V
    },
    // eslint-disable-next-line vue/require-default-prop
    materialType: {
      type: [Number]
    },
    // eslint-disable-next-line vue/require-default-prop
    value: {
      type: [Number, String]
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
    showAll: {
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
    default: {
      type: Boolean,
      default: true
    },
    placeholder: {
      type: String,
      default: '请选择单体'
    }
  },
  data() {
    return {
      loading: false,
      selectValue: undefined,
      options: [],
      originOptions: []
    }
  },
  computed: {
    currentProjectId() {
      return this.projectId || this.globalProjectId
    }
  },
  watch: {
    currentProjectId: {
      handler(val) {
        this.fetch()
      }
    },
    value: {
      handler(val) {
        this.selectValue = val
      },
      immediate: true
    },
    selectValue: {
      handler(val) {
        this.selectChange(val)
      }
    }
  },
  created() {
    this.fetch()
  },
  methods: {
    initVal() {
      this.options = []
    },
    selectChange(val) {
      let monomerVal = {}
      if (!val) {
        val = undefined
        monomerVal = {}
      } else {
        monomerVal = this.originOptions.find(k => k.id === val)
      }
      const areaInfo = monomerVal && monomerVal.AreaSimpleList ? monomerVal.AreaSimpleList : []
      this.$emit('update:value', val)
      this.$emit('change', val)
      this.$emit('getAreaInfo', areaInfo)
    },
    getOptions() {
      return this.options
    },
    async fetch() {
      this.initVal()
      if (!this.projectId) {
        return
      }
      let options = []
      this.loading = true
      try {
        const { content = [] } = await getAll(this.projectId) || {}
        this.originOptions = content || []
        options = content.map(o => {
          return {
            value: o.id,
            label: o.name
          }
        })
      } catch (error) {
        console.log('获取单体列表', error)
      } finally {
        this.options = options
        if (this.default && this.$isNotBlank(options)) {
          this.selectValue = options[0].value
        } else {
          this.selectValue = undefined
        }
        this.loading = false
      }
    }
  }
}
</script>
