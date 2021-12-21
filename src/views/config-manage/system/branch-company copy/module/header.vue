<template>
  <div>
    <div v-show="crud.props.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入公司名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <el-input
        v-model="query.socialCode"
        placeholder="输入社会统一信用代码搜索"
        class="filter-item"
        style="width: 220px;"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <common-select
        :value.sync="query.enabled"
        :options="enabledEnum"
        type="enum"
        clearable
        placeholder="选择状态搜索"
        style="width: 200px;"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation :crud="crud" />
    </div>
    <crudOperation :permission="permission" />
  </div>
</template>

<script>
import { header } from '@crud/crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { enabledEnum } from '@/utils/enum/index'

const defaultQuery = { name: undefined, enabled: enabledEnum.TRUE.V }
export default {
  components: { rrOperation, crudOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  data() {
    return {
      enabledEnum
    }
  }
}
</script>
