import { createProdMockServer } from 'vite-plugin-mock/es/createProdMockServer'
import userMock from '@/api-mock/user.v1'

export function setupProdMockServer() {
  createProdMockServer([...userMock])
}
