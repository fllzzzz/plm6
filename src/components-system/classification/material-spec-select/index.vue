<template>
  <div id="material-spec-select" class="material-spec-select">
    <div class="cls-container">
      <div class="container-prepend">科目</div>
      <material-cascader
        class="cls-cascader"
        v-model="curClsId"
        separator=" > "
        show-all-levels
        clearable
        size="small"
        style="width: 250px"
      />
    </div>
    <div class="input-container">
      <hamburger :is-active="extraQueryOpened" class="hamburger-container" @toggleClick="extraQueryOpened = !extraQueryOpened" />
      <el-input v-model="query.spec" placeholder="可输入规格查询">
        <template #prepend>规格</template>
      </el-input>
    </div>
    <div v-if="extraQueryOpened">
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
    </div>
    <div v-if="matCls.specList" class="tag-container" :style="tagContainerStyle">
      <el-tag
        v-for="item in specList"
        :key="item.sn"
        :type="selected[item.sn] ? 'success' : 'info'"
        size="medium"
        :effect="selected[item.sn] ? '' : 'plain'"
        @click="selectSpec(item)"
        >{{ item.spec }}</el-tag
      >
    </div>
    <el-tag class="tip-tag loading-tag" v-else-if="!loaded" size="medium" effect="plain">
      <el-icon class="is-loading"><el-icon-loading /></el-icon>
      科目加载中
    </el-tag>
    <el-tag class="tip-tag" v-else-if="curClsId" type="danger" size="medium" effect="plain"> * 当前科目未配置规格</el-tag>
    
    <el-tag class="tip-tag" v-else type="warning" size="medium" effect="plain"> * 请先选择科目</el-tag>
  </div>
</template>

<script setup>
import { ref, watch, defineProps, defineEmits, computed } from 'vue'
import { isNotBlank, isBlank } from '@/utils/data-type'
import { getTextDomWidth } from '@/utils/element'
import { getStyle, style2Num } from '@/utils/element/style'
import useMatClsSpec from '@compos/store/use-mat-cls-spec'
import Hamburger from '@comp/Hamburger/index.vue'
import * as lodash from 'lodash'
import materialCascader from '../material-cascader/index.vue'

const emit = defineEmits(['selectionChange', 'update:classifyId'])

const query = ref({
  spec: undefined,
})
const props = defineProps({
  classifyId: {
    type: Number,
  },
})

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
          extraFlag = extraFlag && v.specMap.get(config.id) === query.value[config.id]
        }
      })
      return specFlag && extraFlag
    })
  }
  return []
})

const tagContainerStyle = computed(() => {
  // 动态计算tag的宽度
  // TODO: 当规格长度相差过大时会有问题
  if (isNotBlank(matCls.value.specList)) {
    const actualSpacing = 10
    const tagWidth = getTextDomWidth(matCls.value.specList[0].spec, {
      attribute: new Map([['class', 'el-tag el-tag--info el-tag--medium el-tag--plain']]),
    })
    const dom = document.getElementById('material-spec-select')
    const domWidth = style2Num(getStyle(dom, 'width'))
    let number = Math.floor(domWidth / tagWidth)
    const spacing = domWidth % tagWidth
    if (number > 1 && spacing / number < actualSpacing) {
      number -= 1
    }
    const actualWidth = (domWidth - (number - 1) * actualSpacing) / number
    return {
      'grid-template-columns': `repeat(${number}, ${actualWidth}px)`,
    }
  }
  return { height: '1000px' }
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

function selectSpec(spec) {
  selected.value[spec.sn] = !selected.value[spec.sn]
  const selectSns = Object.keys(selected.value).filter((k) => selected.value[k] === true)
  const selectList = []
  // 获取选中的规格对象
  selectSns.forEach((sn) => {
    selectList.push(lodash.cloneDeep(matCls.value.specMap[sn]))
  })
  emit('selectionChange', selectList)
}
</script>

<style lang="scss" scoped>
.material-spec-select {
  margin: 10px 0;

  .input-container {
    position: relative;
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

  .select-container,
  .cls-container {
    display: flex;
    align-items: center;
    margin-top: 10px;
    width: 100%;
    .container-prepend {
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

    .cls-cascader {
      flex: 1 1 auto;
    }
  }
  .tag-container {
    margin-top: 10px;
    display: grid;
    grid-row-gap: 10px;
    grid-column-gap: 10px;
    // grid-template-columns: 48% 48%;
    justify-content: stretch;
    .el-tag {
      text-align: center;
    }
  }
  .tip-tag {
    width: 100%;
    margin-top: 10px;
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
