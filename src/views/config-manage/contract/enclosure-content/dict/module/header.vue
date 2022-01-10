<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.type"
      :options="TechnologyTypeEnum.ENUM"
      type="enum"
      :disabled-val="[TechnologyTypeEnum.STRUCTURE.V]"
      class="filter-item"
      @change="techChange"
    />
  </div>
  <crudOperation v-if="query.type===TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V">
    <template #optRight>
      <el-popconfirm title="确定删除吗？" @confirm="deleteCode">
        <template #reference>
          <common-button type="danger" size="mini" :disabled="props.selectArr.length<=0">删除</common-button>
        </template>
      </el-popconfirm>

    </template>
  </crudOperation>
</template>

<script setup>
import { TechnologyTypeEnum } from '@enum-ms/contract'
import { defineEmits, defineProps } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { delTrussCode } from '@/api/contract/enclosure-config/enclosure'

const defaultQuery = {
  type: TechnologyTypeEnum.SANDWICH_BOARD.V
}

const { crud, query, CRUD } = regHeader(defaultQuery)
const emit = defineEmits(['type-change'])
const props = defineProps({
  selectArr: {
    type: Array,
    default: () => []
  }
})
function techChange() {
  crud.toQuery()
  emit('type-change', {})
}

async function deleteCode() {
  const submitData = []
  props.selectArr.map(v => {
    submitData.push(v.code)
  })
  try {
    await delTrussCode(submitData)
    crud.notify('删除成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    techChange()
  } catch (e) {
    console.log('删除失败', e)
  }
}
</script>
