<template>
  <div id="material-spec-select" class="material-spec-select" :style="{ 'max-height': `${props.maxHeight}px` }">
    <div class="operate-container">
      <div class="cls-container">
        <div class="container-prepend">科目</div>
        <material-cascader class="cls-cascader" v-model="curClsId" separator=" > " show-all-levels clearable size="small" />
      </div>
      <div class="input-container">
        <hamburger :is-active="extraQueryOpened" class="hamburger-container" @toggleClick="extraQueryOpened = !extraQueryOpened" />
        <el-input v-model="query.spec" placeholder="可输入规格查询">
          <template #prepend>规格</template>
        </el-input>
      </div>
      <div v-if="extraQueryOpened">
        <!-- 型材没有规格配置 -->
        <template v-if="matCls.basicClass !== materialClassificationEnum.SECTION_STEEL.V">
          <template v-for="item in matCls.specConfig" :key="item.id">
            <div class="select-container">
              <div class="container-prepend">{{ item.name }}</div>
              <common-select
                v-model="query[item.id]"
                :options="item.list"
                :loading="!loaded"
                :data-structure="{ key: 'name', label: 'name', value: 'name' }"
                clearable
                filterable
                type="other"
                size="small"
                :placeholder="item.name"
                class="spec-conf-select"
              />
            </div>
          </template>
        </template>
      </div>
    </div>
    <div class="spec-container">
      <div v-if="matCls.specList" class="tag-container" :style="tagContainerStyle">
        <template v-if="props.mode === 'accumulator'">
          <template v-for="item in specList" :key="item.sn">
            <div class="tag-content">
              <el-badge :value="selected[item.sn]" :max="99" class="badge pointer" type="warning" @dblclick="handleClear(item)" />
              <el-tag
                :type="selected[item.sn] ? 'success' : 'info'"
                size="medium"
                :effect="selected[item.sn] ? undefined : 'plain'"
                @click.self="handleAccChange(item)"
                class="pointer"
              >
                {{ item.spec }}
              </el-tag>
            </div>
          </template>
        </template>
        <template v-if="props.mode === 'selector'">
          <template v-for="item in specList" :key="item.sn">
            <el-tag
              :type="selected[item.sn] ? 'success' : 'info'"
              size="medium"
              :effect="selected[item.sn] ? undefined : 'plain'"
              @click="handleSelectChange(item)"
              class="pointer"
            >
              {{ item.spec }}
            </el-tag>
          </template>
        </template>
      </div>
      <el-tag class="tip-tag loading-tag" v-else-if="!loaded" size="medium" effect="plain">
        <el-icon class="is-loading"><el-icon-loading /></el-icon>
        科目加载中
      </el-tag>
      <el-tag class="tip-tag" v-else-if="curClsId" type="danger" size="medium" effect="plain"> * 当前科目未配置规格</el-tag>

      <el-tag class="tip-tag" v-else type="warning" size="medium" effect="plain"> * 请先选择科目</el-tag>
    </div>
  </div>
</template>

<script setup>
import { ref, watch, defineProps, defineEmits, defineExpose, computed, onMounted, onBeforeUnmount } from 'vue'
import { materialClassificationEnum } from '@enum-ms/classification'
import { isNotBlank, isBlank } from '@/utils/data-type'
import { getTextDomWidth } from '@/utils/element'
import { getStyle, style2Num } from '@/utils/element/style'
import useMatClsSpec from '@compos/store/use-mat-cls-spec'
import Hamburger from '@comp/Hamburger/index.vue'
import materialCascader from '../material-cascader/index.vue'

const emit = defineEmits(['accumulateChange', 'selectionChange', 'update:classifyId'])

const props = defineProps({
  classifyId: {
    type: Number
  },
  mode: {
    type: String,
    default: 'accumulator' // accumulator
  },
  maxHeight: {
    type: Number
  }
})

const query = ref({
  spec: undefined
})

const tagContainerStyle = ref({})
const extraQueryOpened = ref(false) // 规格配置查询打开
const matCls = ref({})
const selected = ref({})
const curClsId = ref()

const { loaded, matClsSpec, fetchMatClsSpec } = useMatClsSpec()

// 筛选后的规格列表
const specList = computed(() => {
  if (isNotBlank(matCls.value.specList)) {
    return matCls.value.specList.filter((v) => {
      // 查询规格
      const specFlag = isBlank(query.value.spec) || v.spec.indexOf(query.value.spec) > -1
      // 根据规格配置查询，例如：直径、长度
      let extraFlag = true
      matCls.value.specConfig.forEach((config) => {
        if (isNotBlank(query.value[config.id])) {
          extraFlag = extraFlag && v.specKV[config.id] === query.value[config.id]
        }
      })
      return specFlag && extraFlag
    })
  }
  return []
})

// 监听科目id 变化
watch(
  () => props.classifyId,
  (val) => {
    curClsId.value = val
  },
  { immediate: true }
)

watch(
  curClsId,
  (val) => {
    getSpec(val)
  },
  { immediate: true }
)

watch(
  () => matCls.value.specList,
  (val) => {
    calcWidth()
  }
)

onMounted(() => {
  window.addEventListener('resize', calcWidth, { passive: false })
})

onBeforeUnmount(() => {
  window.removeEventListener('resize', calcWidth)
})

// 获取规格
function getSpec(classifyId) {
  if (classifyId) {
    // 加载科目
    fetchMatClsSpec(classifyId)
    matCls.value = matClsSpec.value[classifyId]
  } else {
    matCls.value = {}
  }
  emit('update:classifyId', classifyId)
}

/**
 * selector 模式
 */
function handleSelectChange(spec) {
  selected.value[spec.sn] = !selected.value[spec.sn]
  const stauts = selected.value[spec.sn]
  if (!stauts) {
    delete selected.value[spec.sn]
  }
  emit('selectionChange', {
    snList: Object.keys(selected.value), // sn 列表
    addList: stauts ? [spec.sn] : [], // 添加列表
    cancelList: !stauts ? [spec.sn] : [] // 删除列表
  })
}

/**
 * accumulator 模式
 */
function handleAccChange(spec) {
  if (isBlank(selected.value[spec.sn])) {
    selected.value[spec.sn] = 1
  } else {
    selected.value[spec.sn]++
  }
  emit('accumulateChange', {
    snList: Object.keys(selected.value), // sn 列表
    addList: [spec.sn], // 添加列表
    cancelList: [] // 删除列表
  })
}

/**
 * accumulator 模式下，减少
 */
function accReduce(sn) {
  if (isNotBlank(selected.value[sn])) {
    selected.value[sn] -= 1
  }
  if (selected.value[sn] === 0) {
    delete selected.value[sn]
  }
}

/**
 * 清除所有列表
 */
function handleClear(spec) {
  delete selected.value[spec.sn]
  emit('accumulateChange', {
    snList: Object.keys(selected.value), // sn 列表
    cancelList: [spec.sn] // 删除列表
  })
}

function init() {
  selected.value = {}
  curClsId.value = undefined
}

// 计算tag宽度
function calcWidth() {
  if (isNotBlank(matCls.value.specList)) {
    // tag间距
    const actualSpacing = 10
    // 为避免出现规格长宽差别稍大的情况，每个tag额外增加20px。
    // TODO: 这样处理仍会有字符串过长，超出tag的情况，暂时未找到好的方式处理
    const tagWidth =
      20 +
      getTextDomWidth(matCls.value.specList[0].spec, {
        attribute: new Map([['class', 'el-tag el-tag--info el-tag--medium el-tag--plain']])
      })
    const dom = document.getElementById('material-spec-select')
    const domWidth = style2Num(getStyle(dom, 'width')) - 10
    let number = Math.floor(domWidth / tagWidth)
    const spacing = domWidth % tagWidth
    if (number > 1 && spacing / number < actualSpacing) {
      number -= 1
    }
    const actualWidth = (domWidth - (number - 1) * actualSpacing) / number
    tagContainerStyle.value = {
      'grid-template-columns': `repeat(${number}, ${actualWidth}px)`
    }
  } else {
    tagContainerStyle.value = {}
  }
}

defineExpose({
  accReduce,
  init
})
</script>

<style lang="scss" scoped>
.material-spec-select {
  display: flex;
  flex-direction: column;
  justify-self: flex-start;
  align-items: flex-start;
  min-height: 400px;
  min-width: 200px;
  .operate-container {
    padding-right: 10px;
    width: 100%;
    flex: 0 0 auto;
  }
  .spec-container {
    padding-right: 10px;
    padding-top: 8px;
    width: 100%;
    overflow: auto;
    flex: 1 1 auto;
  }
  .spec-container::-webkit-scrollbar {
    width: 0 !important;
  }
  .input-container {
    position: relative;
    margin-bottom: 10px;
    .hamburger-container {
      position: absolute;
      padding: 0;
      left: -20px;
      top: 5px;
      transform: scale(0.9);
      opacity: 0.5;
    }
  }

  ::v-deep(.el-input.is-focus .el-input__inner) {
    border-color: #f79b4c;
  }
  ::v-deep(.el-input__inner:focus) {
    border-color: #f79b4c;
  }
  ::v-deep(.el-input-group__prepend) {
    background-color: #f79b4c;
    color: white;
    border: 1px solid #f79b4c;
  }

  .cls-container {
    margin-bottom: 10px;
    ::v-deep(.el-input input) {
      border-top-left-radius: 0;
      border-bottom-left-radius: 0;
    }
  }

  .select-container {
    margin-bottom: 10px;
  }

  .select-container,
  .cls-container {
    display: flex;
    align-items: center;
    width: 100%;
    .container-prepend {
      user-select: none;
      font-size: 13px;
      flex: 0 0 auto;
      height: 32px;
      padding: 0 15px;
      line-height: 32px;
      min-width: 68px;
      text-align: center;
      white-space: nowrap;
      background-color: #f79b4c;
      color: white;
      border: 1px solid #f79b4c;
      border-radius: 4px;
      border-right: 0;
      border-top-right-radius: 0;
      border-bottom-right-radius: 0;
    }
    .spec-conf-select {
      flex: 1 1 auto;
      ::v-deep(.el-input__inner) {
        border-top-left-radius: 0;
        border-bottom-left-radius: 0;
      }
    }

    ::v-deep(.cls-cascader) {
      width: 100%;
      flex: 1 1 auto;
    }
  }
  .tag-container {
    display: grid;
    grid-row-gap: 10px;
    grid-column-gap: 10px;
    // grid-template-columns: 48% 48%;
    justify-content: stretch;

    .tag-content {
      position: relative;
      .el-tag {
        width: 100%;
        user-select: none;
        text-align: center;
      }
      .badge {
        user-select: none;
        position: absolute;
        right: -7px;
        top: -8px;
      }
    }
  }
  .tip-tag {
    width: 100%;
    .is-loading {
      line-height: 5px;
    }
  }
  .loading-tag {
    display: inline-flex;
    align-items: center;
    .is-loading {
      margin-right: 5px;
    }
  }
}
</style>
