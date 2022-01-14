<template>
  <span>
    <el-button-group class="button-group">
      <el-button class="input-button" :size="props.size" :type="props.btnType" plain>
        <tableTemplateCascader
          v-if="isCascader"
          v-model:value="templateId"
          :table-types="tableType"
          :size="props.size"
          :disabled="props.disabled"
          :cache-template-id="cacheTemplateId"
          initial
          filterable
          default
          @change="handleTemplateSelect"
          @onload="templateOnload = true"
        />
        <tableTemplateSelect
          v-else
          v-model:value="templateId"
          :table-type="tableType"
          :size="props.size"
          :disabled="props.disabled"
          :cache-template-id="cacheTemplateId"
          default
          filterable
          @change="handleTemplateSelect"
          @onload="templateOnload = true"
        />
      </el-button>
      <el-button :size="props.size" :disabled="!templateOnload || props.disabled" icon="el-icon-view" :type="props.btnType" plain @click="print(printModeEnum.PREVIEW.V)" />
      <el-button :size="props.size" :disabled="!templateOnload || props.disabled" icon="el-icon-printer" :type="props.btnType" plain @click="print(printModeEnum.QUEUE.V)" />
      <el-button :size="props.size" :disabled="!templateOnload || props.disabled" icon="el-icon-download" :type="props.btnType" plain @click="download" />
    </el-button-group>
  </span>
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits } from 'vue'
import { ElButtonGroup, ElButton } from 'element-plus'
import { mapGetters } from '@/store/lib'

import storage from '@/utils/storage'
import fetchFn from '@/utils/print/api'
import { isNotBlank } from '@data-type/index'
import { printTable } from '@/utils/print/table'
import formatFn from '@/utils/print/format/index'
import { apikey } from '@/utils/print/table/type'
import { printModeEnum } from '@/utils/print/enum'
import { ElLoading, ElMessage } from 'element-plus'
import { uniqueArr } from '@/utils/data-type/array'
import downloadXLSX from '@/utils/print/download'
import tableTemplateSelect from '@comp-common/print/table-template-select'
import tableTemplateCascader from '@comp-common/print/table-template-cascader'

const emit = defineEmits(['update:currentKey', 'change'])

const props = defineProps({
  /**
     * api_key, 当模板只对应一个接口时，api_key === table_key(table_type)
     * 当一个打印下，有多个apikey，且apikey对应的key相同时，默认会选择第一个apikey打印
     */
  apiKey: {
    type: [Array, String],
    default: undefined
  },
  currentKey: { // 当前使用apikey
    type: String,
    default: undefined
  },
  beforePrint: { // 打印前调用的方法
    type: Function,
    default: undefined
  },
  size: {
    type: String,
    default: 'mini'
  },
  disabled: {
    type: Boolean,
    default: false
  },
  params: { // 打印参数，传入数组则批量打印
    type: null,
    default: undefined
  },
  btnType: {
    type: String,
    default: 'warning'
  }
})

const config = ref({})
const templateId = ref(undefined) // 当前选择的模板id
const cacheTemplateId = ref(undefined)// 用户上次使用的模板id
const templateOnload = ref(false)

const { user } = mapGetters('user')

const userId = computed(() => {
  return user.value && user.value.id
})

const tableType = computed(() => { // table_key
  let _tableType = ''
  if (isNotBlank(props.apiKey)) {
    if (props.apiKey instanceof Array) {
      if (props.apiKey.length === 1) {
        _tableType = props.apiKey[0]
      } else {
        _tableType = props.apiKey
        const _t = uniqueArr(_tableType.map(v => { // 转换后去重
          return apikey[v] || v
        }))
        if (_t.length > 1) {
          return _t
        } else {
          return _t[0]
        }
      }
    } else {
      _tableType = props.apiKey
    }
  }
  return apikey[_tableType] || _tableType
})

const isCascader = computed(() => {
  return tableType.value instanceof Array
})

const api_key = computed(() => { // api_key
  if (isNotBlank(config.value) && isNotBlank(config.value.type)) {
    const currentTableType = config.value.type
    if (props.apiKey instanceof Array) {
      for (const _key of props.apiKey) {
        if (_key === currentTableType || apikey[_key] === currentTableType) {
          return _key
        }
      }
    } else {
      return props.apiKey
    }
  }
  return undefined
})

watch(
  () => props.apiKey,
  (value) => {
    let key = ''
    if (isNotBlank(value)) {
      key = value instanceof Array ? value[0] : value
    }
    cacheTemplateId.value = getCacheTemplateId(key)
    templateOnload.value = true
  },
  { immediate: true }
)

watch(
  () => api_key.value,
  (value) => {
    selectChange(value)
  }
)

function selectChange(val) {
  emit('update:currentKey', val)
  emit('change', val)
}

function handleTemplateSelect(val) {
  if (isNotBlank(val)) {
    config.value = typeof val === 'object' ? JSON.parse(JSON.stringify(val)) : JSON.parse(val)
  } else {
    config.value = ''
  }
}
// 获取用户打印的模板id
function getCacheTemplateId(apikey) {
  let templateId = ''
  if (isNotBlank(userId.value) && isNotBlank(apikey)) {
    const printTableCache = storage.get(`printTable_${userId.value}`) || {}
    templateId = printTableCache[apikey]
  }
  return templateId
}

// 缓存用户打印的模板id
function setUserCache(apikey, templateId) {
  if (isNotBlank(userId.value) && isNotBlank(apikey) && isNotBlank(templateId)) {
    const printTableCache = storage.get(`printTable_${userId.value}`) || {}
    printTableCache[apikey] = templateId
    storage.set(`printTable_${userId.value}`, printTableCache)
  }
}
// 下载
async function download() {
  // beforePrint 返回 false 停止打印
  if (typeof props.beforePrint !== 'function' || props.beforePrint() !== false) {
    setUserCache(api_key.value, templateId.value)
    const printLoading = ElLoading.service({
      lock: true,
      text: '准备导出',
      spinner: 'el-icon-loading',
      fullscreen: true
    })
    try {
      // 传入数组批量导出
      let params = props.params
      if (!(props.params instanceof Array)) {
        params = [props.params]
      }
      for (const p of params) {
        printLoading.text = `正在加载数据：${config.value.name}`
        const { header, footer, table, qrCode } = await fetch(p) || {}
        printLoading.text = `正在导出：${config.value.name}`
        const result = await downloadXLSX({
          header, footer, table, qrCode,
          config: config.value
        })
        if (!result) {
          throw new Error('导出失败')
        }
      }
    } catch (error) {
      ElMessage.error(error)
      console.log('导出', error)
    } finally {
      printLoading.text = `导出结束`
      printLoading.close()
    }
  }
}

// 打印
async function print(printMode) {
  // beforePrint 返回 false 停止打印
  if (typeof props.beforePrint !== 'function' || props.beforePrint() !== false) {
    setUserCache(api_key.value, templateId.value)
    const printLoading = ElLoading.service({
      lock: true,
      text: '准备打印',
      spinner: 'el-icon-loading',
      fullscreen: true
    })
    try {
      // 传入数组批量打印
      let params = props.params
      if (!(props.params instanceof Array)) {
        params = [props.params]
      }
      // TODO:如果是预览则只取第一条, 批量预览日后优化
      if (printMode === printModeEnum.PREVIEW.V) {
        params = [params[0]]
      }
      for (const p of params) {
        printLoading.setText(`正在加载数据：${config.value.name}`)
        const { header, footer, table, qrCode } = await fetch(p) || {}
        printLoading.setText(`正在打印：${config.value.name}`)
        const result = await printTable({
          printMode,
          header, footer, table, qrCode,
          config: config.value
        })
        if (!result) {
          throw new Error('打印失败')
        }
      }
    } catch (error) {
      ElMessage.error(error)
      console.log('打印', error)
    } finally {
      printLoading.text = `打印结束`
      printLoading.close()
    }
  }
}

async function fetch(params) {
  const key = api_key.value
  const tableKey = config.value.type
  if (!key) {
    return
  }
  try {
    const data = await fetchFn[key](params) || {}
    if (formatFn[tableKey]) { // 数据装换
      return formatFn[tableKey](data)
    } else {
      return data
    }
  } catch (error) {
    throw new Error('加载打印数据失败')
  }
}
</script>

<style lang="scss" scoped>
.button-group{
  display: flex;
  width: 100%;
  .input-button {
    flex: auto;
    padding: 0;
    >span:first-child >div:first-child{
      width: 100%;
    }
    ::v-deep(.el-input--mini .el-input__inner){
      height: 27px;
      line-height: 27px;
    }
    ::v-deep(.el-input--small .el-input__inner) {
      height: 31px;
      line-height: 31px;
    }
    ::v-deep(.el-input--medium .el-input__inner) {
      height: 35px;
      line-height: 35px;
    }
    ::v-deep(.el-input__inner){
      border: none;
      width: 100%;
    }
    .el-cascader--mini {
      line-height: 27px;
    }
    .el-cascader--small {
      line-height: 31px;
    }
    .el-cascader--medium {
      line-height: 35px;
    }
  }
  ::v-deep(.input-button.el-button.is-plain) {
    &:active,&:hover,&:focus{
      background-color: unset!important;
      color: #fff;
      outline: none;
    }
  }
}
</style>
