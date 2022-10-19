<template>
  <div>
    <div v-show="!groupId">
      <div class="my-code">点击生产组查看详情</div>
    </div>
    <div v-show="groupId">
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="!loaded"
        :data="list"
        :data-format="dataFormat"
        :max-height="maxHeight + 135"
        :default-expand-all="false"
        :expand-row-keys="expandRowKeys"
        row-key="id"
        style="width: 100%"
      >
        <el-expand-table-column :data="list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
          <template #default="{ row }">
            <!-- <p>
              计价方式：<span>{{ row.wageQuotaType }}</span>
            </p> -->
            <p>
              组员：<span>{{ row.leaderName }}</span>
            </p>
          </template>
        </el-expand-table-column>
        <el-table-column key="processName" prop="processName" :show-overflow-tooltip="true" label="工序名称" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.processName }}</span>
          </template>
        </el-table-column>
        <!-- <el-table-column key="wageQuotaType" prop="wageQuotaType" label="计价方式" align="center" width="75px">
          <template #default="{ row }">
            <span>{{ row.wageQuotaType }}</span>
          </template>
        </el-table-column> -->
        <!-- <el-table-column key="organizationType" prop="organizationType" label="属性" align="center" width="65px">
          <template v-slot="scope">
            {{ teamAttributeEnum.VL[scope.row.organizationType] }}
          </template>
        </el-table-column> -->
        <el-table-column key="leaderName" prop="leaderName" label="组长" min-width="100px" />
        <!-- <el-table-column key="memberNames" prop="memberNames" :show-overflow-tooltip="true" label="组员" min-width="160px" /> -->
      </common-table>
      <common-dialog
        title="选择班组"
        v-model="dialogVisible"
        :before-close="
          () => {
            dialogVisible = false
          }
        "
        :close-on-click-modal="false"
        width="500px"
      >
        <template #titleRight>
          <common-button :loading="submitLoading" :disabled="!selectValue?.length" size="mini" type="primary" @click="submitIt">
            保存
          </common-button>
        </template>
        <common-select
          v-model="selectValue"
          :options="productionTeamOptions"
          :type="'other'"
          multiple
          filterable
          clearable
          :dataStructure="{ key: 'id', label: 'label', value: 'id' }"
          placeholder="请选择班组"
          style="width: 100%"
        >
          <!-- <template #view="{ data: item }">
            <span>{{ item.leaderName }} | {{ item.processName }} | {{ teamAttributeEnum.VL[item.organizationType] }}</span>
          </template> -->
        </common-select>
      </common-dialog>
    </div>
  </div>
</template>

<script setup>
import { productAddTeam } from '@/api/mes/production-config/production-line-group'
import { defineProps, defineExpose, ref, defineEmits, watch, computed, inject } from 'vue'
// import { teamAttributeEnum, wageQuotaTypeEnum } from '@enum-ms/mes'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { cleanArray } from '@data-type/array'

import useProductionTeam from '@compos/store/use-production-team'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { ElNotification } from 'element-plus'

const dataFormat = [['wageQuotaType', ['parse-enum', wageQuotaTypeEnum, { f: 'SL', extra: '计价' }]]]

const maxHeight = inject('maxHeight')
// 展开keys
const expandRowKeys = ref([])

const { loaded, productionTeamKV, productionTeam } = useProductionTeam()
const selectValue = ref([])
const dialogVisible = ref(false)
const submitLoading = ref(false)

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: Array,
    default: () => []
  },
  line: {
    type: Object,
    default: () => {}
  },
  group: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  },
  { immediate: true }
)

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      selectValue.value = props.modelValue
    }
  }
)

const groupId = computed(() => {
  return props.group && props.group.id
})

const list = computed(() => cleanArray(props.modelValue.map((v) => productionTeamKV.value[v])))

const productionTeamOptions = computed(() => productionTeam.value.filter((v) => props.line?.productType & v.productType && props.line?.productionLineTypeEnum & v.productionLineTypeEnum))

async function submitIt() {
  try {
    submitLoading.value = true
    await productAddTeam({
      groupId: groupId.value,
      teamIds: selectValue.value
    })
    ElNotification({
      title: '班组绑定成功',
      type: 'success',
      duration: 2500
    })
    emit('update:modelValue', selectValue.value)
    emit('change', selectValue.value)
  } catch (error) {
    console.log(error, '绑定班组')
  } finally {
    dialogVisible.value = false
    submitLoading.value = false
  }
}

defineExpose({
  toAdd: () => (dialogVisible.value = true)
})
</script>
